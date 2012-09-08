(ns deuce.emacs-lisp.parser
  (require [clojure.walk :as w]
           [clojure.string :as s]
           [deuce.emacs-lisp :as el])
  (import [java.util Scanner]
          [java.io StringReader StreamTokenizer]
          [java.util.regex Pattern]
          [deuce.emacs_lisp DottedPair]))

(declare tokenize)

(def character-modifier-bits {"\\A" 0x0400000
                              "\\s" 0x0800000
                              "\\H" 0x1000000
                              "\\S" 0x2000000
                              "\\C" 0x4000000
                              "\\M" 0x8000000})

(defn ^:private parse-string [s]
  (.sval (doto (StreamTokenizer. (StringReader. (s/replace s "\n" "\\\n")))
           (.nextToken))))

(defn ^:private parse-character [c]
  (let [parts (if (= "-" c) [c] (s/split c #"-"))
        [mods c] [(set (butlast parts)) (last parts)]
        c (cond
           (re-find #"\\\^(.)" c) (- (int (last c)) 64)
           (mods "\\C") (- (int (first (s/upper-case c))) 64)
           :else (int (first (parse-string (str \" c \")))))]
    (reduce bit-xor c (map character-modifier-bits (disj mods "\\C")))))

(def ^:private ^Pattern re-str #"(?s)([^\"\\]*(?:\\.[^\"\\]*)*)\"")

(def ^:dynamic line (atom 1))

(defn ^:private tokenize-all [^Scanner sc]
  (take-while identity (repeatedly (partial tokenize sc))))

(defn ^:private tokenize [^Scanner sc]
  (let [find (fn [^Pattern re h] (.findWithinHorizon sc re (int h)))]
    (condp find 1
      #"\n" (do (swap! line inc) (recur sc))
      #"\s" (recur sc)
      #"[)\]]" nil
      #"\(" (with-meta (tokenize-all sc) {:line @line})
      #"\[" (with-meta (list 'quote (vec (tokenize-all sc))) {:line @line})
      #"," (list (if (find #"@" 1) (symbol "\\,@") (symbol "\\,")) (tokenize sc))
      #"'" (list 'quote (tokenize sc))
      #"`" (let [form (tokenize sc)] (if (symbol? form)
                                       (list 'quote form)
                                       (list (symbol "\\`") form)))
      #":" (keyword (.next sc))
      #"\." DottedPair
      #"\?" (parse-character (.next sc))
      #"\"" (parse-string (str \" (find re-str 0)))
      #";" (do (swap! line inc) (list 'clojure.core/comment (.nextLine sc)))
      #"#" (condp find 1
             #"'" (list 'var (tokenize sc))
             #"\(" (let [[object start end properties] (tokenize-all sc)]
                     (list 'set-text-properties start end properties object))
             #"x" (.nextInt sc (int 16))
             #"o" (.nextInt sc (int 8))
             #"b" (.nextInt sc (int 2))
             (when (.hasNext sc #"\d+r\S+")
               (let [radix (find #"\d+" 0)]
                 (find #"r" 1)
                 (.nextInt sc (Integer/parseInt radix)))))
      (cond
       (.hasNextLong sc) (.nextLong sc)
       (.hasNextDouble sc) (.nextDouble sc)
       (.hasNext sc) (let [s (.next sc)]
                       (with-meta (symbol (if (= "/" s)
                                            s
                                            (s/replace s "/" "_SLASH_"))) {:line @line}))))))

(defn ^:private expand-dotted-pairs [form]
  (if (and (list? form) (= 3 (count form)) (= DottedPair (second form)))
    (DottedPair. (first form) (last form))
    form))

(def ^:private clojure-syntax-quote
  (doto
      (.getDeclaredMethod clojure.lang.LispReader$SyntaxQuoteReader
                          "syntaxQuote"
                          (into-array [Object]))
    (.setAccessible true)))

(defn ^:private syntax-quote [form]
  (->> form
       (w/postwalk-replace {(symbol "\\,") `unquote
                            (symbol "\\,@") `unquote-splicing})
       (w/postwalk #(if (and (list? %) (= (symbol "\\`") (first %)))
                      (.invoke clojure-syntax-quote nil (into-array [(second %)]))
                      %))))

(defn parse [r]
  (binding [line (atom 1)]
    (->> (tokenize-all (doto (if (string? r) (Scanner. r) (Scanner. r "UTF-8"))
                         (.useDelimiter #"(\s|\]|\)|\"|;)")))
         (w/postwalk expand-dotted-pairs)
         (w/postwalk-replace {(symbol "nil") nil 't true})
         syntax-quote)))
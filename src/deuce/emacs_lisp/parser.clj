(ns deuce.emacs-lisp.parser
  (:require [clojure.walk :as w]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [deuce.emacs-lisp :as el]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.textprop :as textprop])
  (:import [deuce.emacs_lisp.cons Cons]
           [java.util Scanner]
           [java.io StringReader StreamTokenizer]
           [java.util.regex Pattern]))

(declare tokenize)

(def character-modifier-bits {"\\A" 0x0400000
                              "\\s" 0x0800000
                              "\\H" 0x1000000
                              "\\S" 0x2000000
                              "\\C" 0x4000000
                              "\\M" 0x8000000})

(defn ^:private parse-string [s]
  (.sval (doto (StreamTokenizer. (StringReader. (reduce (fn [s [m r]] (s/replace s m r)) s
                                                        [["\\\n" ""]
                                                         ["\n" "\\\n"]])))
           (.nextToken))))

(defn ^:private parse-character [c]
  (let [parts (if (= "-" c) [c] (s/split c #"-"))
        [mods c] [(set (butlast parts)) (last parts)]
        c (cond
           (re-find #"\\\^(.)" c) (- (int (last c)) 64)
           (mods "\\C") (- (int (first (s/upper-case c))) 64)
           :else (int (first (parse-string (str \" c \")))))]
    (reduce bit-xor c (map character-modifier-bits (disj mods "\\C")))))

(defn ^:private strip-comments [form]
  (remove (every-pred seq? (comp `#{comment} first)) form))

(defn ^:private expand-cons [form]
  (if (and  (= 3 (count form)) (= '. (second form)))
    (list 'deuce.emacs.alloc/cons (first form) (last form))
    form))

(defn ^:private as-vector [form]
  (object-array (vec form)))

(def ^:private ^Pattern re-str #"(?s)([^\"\\]*(?:\\.[^\"\\]*)*)\"")

(def ^:dynamic line (atom (int 1)))

(defn ^:private tokenize-all [^Scanner sc]
  (strip-comments (take-while (complement #{`end}) (repeatedly (partial tokenize sc)))))

(defn ^:private tokenize [^Scanner sc]
  (let [find (fn [^Pattern re h] (.findWithinHorizon sc re (int h)))]
    (condp find 1
      #"\n" (do (swap! line inc) (recur sc))
      #"\s" (recur sc)
      #"[)\]]" `end
      #"\(" (with-meta (expand-cons (tokenize-all sc)) {:line @line})
      #"\[" (as-vector (tokenize-all sc))
      #"," (list (if (find #"@" 1) `unquote-splicing `unquote) (tokenize sc))
      #"'" (list 'quote (tokenize sc))
      #"`" (let [form (tokenize sc)] (if (symbol? form)
                                       (list 'quote form)
                                       (list `el/syntax-quote form)))
      #":" (keyword (.next sc))
      #"\?" (parse-character (.next sc))
      #"\"" (let [s (parse-string (str \" (find re-str 0)))]
              (swap! line + (count (butlast (re-seq #"\n" s))))
              s)
      #";" (do (swap! line inc) (list `comment (.nextLine sc)))
      #"#" (condp find 1
             #"'" (list 'quote (tokenize sc))
             #"\(" (let [[object start end properties] (tokenize-all sc)]
                     (list `textprop/set-text-properties start end properties object))
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
                       (case s
                         "t" true
                         "nil" nil
                         (with-meta (symbol nil s) {:line @line})))
       :else `end))))

(def scanner-position (doto (.getDeclaredField Scanner "position")
                        (.setAccessible true)))

(defn parse-internal [r & [all?]]
  (let [scanner (doto (if (string? r) (Scanner. r) (Scanner. r "UTF-8"))
                  (.useDelimiter #"(\s|\]|\)|\"|;)"))]
    (binding [line (atom (int 1))]
      (Cons.
       ((if all? tokenize-all tokenize) scanner)
       (.get scanner-position scanner)))))

(defn parse [r]
  (.car (parse-internal r :all)))

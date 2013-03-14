(ns deuce.emacs-lisp.parser
  (:require [clojure.walk :as w]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [deuce.emacs-lisp :as el]
            [deuce.emacs-lisp.cons :refer [car cdr] :as cons]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.textprop :as textprop])
  (:import [java.util Scanner]
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

;; Various ctrl-characters are broken, many ways they can be specified, this simplified take doesn't fit the Emacs model.
;; Should be rewritten with some thought behind it. Maybe a test.
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Character-Type.html doesn't really cover it in all it's glory.
;; Looks like edmacro/edmacro-parse-keys actually contains a lot of the logic.
;; Like Emacs, certain characters can be read both with single and double backslash. Not necessarily the same ones.

(def emacs-escape-characters {"\\e" \ ;; <ESC>
                              "\r" \return "\\" \\ "\\s" \space
                              "\\C-?" 127 "\\d" 127 ;; <DEL>
                              })

(defn ^:private parse-character [c]
  (if-let [escape-char (emacs-escape-characters c)]
    (int escape-char)
    (let [parts (if (= "-" c) [c] (s/split c #"-"))
          [mods c] [(set (butlast parts)) (last parts)]
          ctrl-char? #(<= (int \A) (+ % 64) (int \Z)) ;; This is where it really starts to go downhill.
          c (cond
             (re-find #"\\\^(.)" c) (- (int (last c)) 64)
             (re-find #"\\\d+" c) (Integer/parseInt (subs c 1) 8)
             (re-find #"\\x\p{XDigit}" c) (Integer/parseInt (subs c 2) 16)
             (mods "\\C") (- (int (first (s/upper-case c))) 64)
             :else (int (first (parse-string (str \" c \")))))]
      (reduce bit-xor c (map character-modifier-bits
                             (if (ctrl-char? c)
                               (disj mods "\\C")
                               mods))))))

(defn ^:private strip-comments [form]
  (remove (every-pred seq? (comp `#{comment} first)) form))

(defn ^:private as-vector [form]
  (object-array (vec form)))

(def ^:private ^Pattern re-str #"(?s)([^\"\\]*(?:\\.[^\"\\]*)*)\"")
(def ^:private ^Pattern re-char #"(?s)((\\[CSMAHs]-)*(\\x?\p{XDigit}+|(\\\^?)?.))")

(defn ^:private tokenize-all [^Scanner sc]
  (strip-comments (take-while (complement #{`end}) (repeatedly (partial tokenize sc)))))

(defn ^:private tokenize [^Scanner sc]
  (let [find (fn [^Pattern re h] (.findWithinHorizon sc re (int h)))]
    (condp find 1
      #"\s" (recur sc)
      #"[)\]]" `end
      #"\(" (tokenize-all sc)
      #"\[" (as-vector (tokenize-all sc))
      #"," (list (if (find #"@" 1) '#el/sym "\\,@" '#el/sym "\\,") (tokenize sc))
      #"'" (list 'quote (tokenize sc))
      #"`" (let [form  (tokenize sc)]
             (if (symbol? form)
               (list 'quote form)
               (list '#el/sym "\\`" form)))
      #":" (keyword (.next sc))
      #"\?" (parse-character (find re-char 0))
      #"\"" (parse-string (str \" (find re-str 0)))
      ;; Deal with: ;;; -*- lexical-binding: t -*- or autoload      ;;;###autoload      ;;; Code
      #";" (list `comment (.nextLine sc))
      #"#" (condp find 1
             #"'" (list 'function (tokenize sc))
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
       (.hasNext sc) (let [s (find #"[^\s\[\]\(\)\"\;]+" 0)]
                       (case s
                         "t" true
                         "nil" nil
                         (symbol nil s)))
       :else `end))))

(def scanner-position (doto (.getDeclaredField Scanner "position")
                        (.setAccessible true)))

(defn parse-internal [r & [all?]]
  (let [scanner (doto (if (string? r) (Scanner. r) (Scanner. r "UTF-8"))
                  (.useDelimiter #"(\s|\]|\)|\"|;)"))]
    (cons/pair
     ((if all? tokenize-all tokenize) scanner)
     (.get scanner-position scanner))))

(defn parse [r]
  (cons/car (parse-internal r :all)))

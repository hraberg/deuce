(ns deuce.emacs-lisp.parser
  (:require [clojure.walk :as w]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [deuce.emacs-lisp :as el]
            [deuce.emacs-lisp.cons :refer [car cdr] :as cons]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.casefiddle :as casefiddle]
            [deuce.emacs.textprop :as textprop])
  (:import [java.util Scanner]
           [java.io StringReader StreamTokenizer]
           [java.util.regex Pattern]))

(declare tokenize)

(def character-modifier-symbols '{"\\A" alt "\\s" super "\\H" hyper
                                  "\\S" shift "\\C" control "\\M" meta})

(def character-modifier-bits '{alt 0x0400000
                               super 0x0800000
                               hyper 0x1000000
                               shift 0x2000000
                               control 0x4000000
                               meta 0x8000000})

(defn ^:private parse-string [s]
  (.sval (doto (StreamTokenizer. (StringReader. (reduce (fn [s [m r]] (s/replace s m r)) s
                                                        [["\\\n" ""]  ["\n" "\\\n"]])))
           (.nextToken))))

;; Like Emacs, certain characters can be read both with single and double backslash. Not necessarily the same ones.
(def emacs-escape-characters {"\\e" \ ;; <ESC>
                              "\r" \return "\\" \\ "\\s" \space
                              "\\C-?" \ "\\d" \ ;; <DEL>
                              "\\^?" \})

;; Various ctrl-characters are broken, many ways they can be specified, this simplified take doesn't fit the Emacs model.
;; Should be rewritten with some thought behind it. Maybe a test.
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Character-Type.html doesn't really cover it in all it's glory.
;; Looks like edmacro/edmacro-parse-keys actually contains a lot of the logic.

;; Here's an attempt at doing something more correct, see deuce.emacs.keyboard/event-convert-list:
(defn event-convert-list-internal [mods base]
  (let [[mods base] [(set mods) (int base)]
        maybe-control (casefiddle/upcase base)
        [mods base] (cond
                     (and (mods 'control) (Character/isISOControl (char base)))
                     [(disj mods 'control) base]

                     (and (mods 'control) (< (- maybe-control (int \@)) (int \space)))
                     [(disj mods 'control) (- maybe-control (int \@))]

                     :else [mods base])
        uppercase (casefiddle/upcase base)
        [mods base] (if (mods 'shift)
                      [(if (not= base uppercase)
                         (disj mods 'shift)
                         mods) uppercase]
                      [mods base])]
    (reduce bit-xor base (replace character-modifier-bits mods))))

(defn ^:private parse-character [c]
  (if-let [escape-char (emacs-escape-characters c)]
    (int escape-char)
    (let [parts (if (= "-" c) [c] (s/split c #"-"))
          [mods c] [(set (butlast parts)) (last parts)]
          c (cond
             (re-find #"\\\^(.)" c) (event-convert-list-internal
                                     '(control) (- (int (casefiddle/upcase (last c))) (int \@)))
             (re-find #"\\\d+" c) (Integer/parseInt (subs c 1) 8)
             (re-find #"\\x\p{XDigit}" c) (Integer/parseInt (subs c 2) 16)
             :else (int (first (parse-string (str \" c \")))))]
      (event-convert-list-internal (replace character-modifier-symbols mods) c))))

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
             ;; #"^" is a CharTable looking like this: #^[nil nil keymap ...]
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

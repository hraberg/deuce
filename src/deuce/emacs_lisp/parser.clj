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

;; In theory escape characters in strings should be escaped inside Emacs strings somewhat like this:
(defn ^:private parse-control-char [maybe-control & [for-string?]]
  (cond
   (> maybe-control 127) (when for-string?
                           (char (- maybe-control 96)))

   (= (int \?) maybe-control) \ ;; DEL

   ;; This case results in a normal xor modifier in event-convert-list-internal by returning nul.
   (or (> (int \?)  maybe-control (int \space))
       (>=  maybe-control (int \{))
       (#{\tab \return \newline} (char maybe-control)))
   (when for-string? (el/throw* 'error "Invalid modifier in string"))

   :else (char (mod maybe-control 32))))

;; See http://www.gnu.org/software/emacs/manual/html_node/elisp/Nonprinting-Characters.html
(defn resolve-control-chars [s]
  (-> s
      (s/replace #"\\d" "") ;; DEL
      (s/replace #"\\e" "") ;; ESC
      (s/replace #"\\s" " ")
      (s/replace #"\\t" "\t")
      (s/replace #"(?s)\\+(?:C-|\^)(\\?.)" ;; Optional backslash handling somewhat confusing.
                 (fn [[control base]]
                   (if (re-find #"^\\\\" control) ;; Can be a quoted control char
                     control
                     (str (parse-control-char (int (last base)) :for-string)))))
      (s/replace #"\\M-(\\?.)"   ;; "\M-i" converts into "á". This only works for 7-bit ASCII.
                 (fn [[meta base]]
                   (let [c (int (last base))]
                     (if (< c 128)
                       (str (char (bit-xor 128 c)))
                       (el/throw* 'error "Invalid modifier in string")))))))

;; This takes an actual quoted String. Can easiest be called from the REPL by chaining (pr-str "...")
(defn ^:private parse-string [s]
  (reduce (fn [s [m r]] (s/replace s m r))
          (subs s 1 (dec (count s)))
          [["\\\n" ""]
           ["\\\"" "\""]
           ["\\\\" "\\"]
           [#"\\(\d+)" (fn [[_ n]]
                         (str (char (Integer/parseInt n 8))))]
           [#"\\x(\p{XDigit}+)" (fn [[_ n]]
                                  (str (char (Integer/parseInt n 16))))]
           ["\\n" "\n"]
           ["\\r" "\r"]
           ["\\b" "\b"]
           ["\\t" "\t"]]))

;; Like Emacs, certain characters can be read both with single and double backslash. Not necessarily the same ones.
(def emacs-problem-chars {"\\" \\ "\\s" \space
                          "\\-" \- "-" \- "\"" \"})

;; Various ctrl-characters are broken, many ways they can be specified, this simplified take doesn't fit the Emacs model.
;; Should be rewritten with some thought behind it. Maybe a test.
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Character-Type.html doesn't really cover it in all it's glory.
;; Looks like edmacro/edmacro-parse-keys actually contains a lot of the logic.

;; Here's an attempt at doing something more correct, see deuce.emacs.keyboard/event-convert-list:
(defn event-convert-list-internal [mods base & [no-modifier-conversion]]    ;; no-modifiers-conversion is used when parsing chars.
  (let [[mods base] [(set mods) (int base)]
        [mods base] (if-let [control-char (and (mods 'control)              ;; This turns '(control \space) into 0: "\^@"
                                               (and (not (<= (int base) (int \space)))
                                                    no-modifier-conversion) ;; Don't reparse actual lower control characters.
                                               (parse-control-char base))]
                      [(disj (if (and (not no-modifier-conversion)          ;; It is a valid 5 bit or 127 control char
                                      (Character/isUpperCase (char base)))  ;; Remove control modifier as its baked in.
                               (conj mods 'shift)                           ;; If original was upper case, add shift modifier
                               mods) 'control) (int control-char)]
                      [mods base])
        [mods base] (if (and (mods 'shift) (not no-modifier-conversion)     ;; Turns '(shift \a) into \A
                             (Character/isLowerCase (char base))            ;; If and only if a 7 bit ASCII char, upper case it
                             (< base 128))
                      [(disj mods 'shift) (casefiddle/upcase base)]         ;; Remove shift modifier as its baked in.
                      [mods base])]                                         ;; (But upper case characters can have 'shift as well.)
    (reduce bit-xor base (replace character-modifier-bits mods))))          ;; XOR in the modifiers.

;; Takes an Emacs-style charcter specifier without the leading ?
;; Turns "C-a" into \
;; Returns characters for things that fit below Character/MAX_VALUE, otherwise ints.
;; We may want our own real type for this. It parses the base character as a string first.
(defn ^:private parse-character [c]
  (if-let [c (emacs-problem-chars c)]
    c
    (let [parts  (if (re-find #".+--$" c)
                   (vec (concat (s/split c #"-") ["-"]))
                   (s/split c #"-"))
          [mods c] [(set (butlast parts)) (last parts)]
          c (cond
             (character-modifier-symbols c) -1
             (re-find #"\\\d+" c) (Integer/parseInt (subs c 1) 8)
             (re-find #"\\x\p{XDigit}+" c) (Integer/parseInt (subs c 2) 16)
             :else (int (first (resolve-control-chars (parse-string (str \" c \"))))))]
      (if (= -1 c) c
          (let [c (event-convert-list-internal
                   (replace character-modifier-symbols mods) c :no-modifier-conversion)]
            (if (<= c (int (Character/MAX_VALUE)))
              (char c)
              (long c)))))))

(defn ^:private strip-comments [form]
  (remove (every-pred seq? (comp `#{comment} first)) form))

(defn ^:private as-vector [form]
  (object-array (vec form)))

(def ^:private ^Pattern re-str #"(?s)([^\"\\]*(?:\\.[^\"\\]*)*)\"")
(def ^:private ^Pattern re-char #"(?s)((\\[CSMAHs]-)*(\\x?\p{XDigit}+|(\\\^?)?.))")

(defn parse-characters [s meta-prefix-char]
  (seq (map (comp parse-character first)
            (re-seq re-char (resolve-control-chars (s/replace s "\\M-" (str (char meta-prefix-char))))))))

(def ^:private unmodifiers (zipmap (vals character-modifier-bits) (keys character-modifier-bits)))

;; Turns #el/vec [67108911] into '(control \/), not sure how to read them from keyboard using lanterna.
(defn unmodify-key [k]
  (let [keys (reduce
              (fn [[key & modifiers :as k] modifier]
                (if (> (bit-and key modifier) 0)
                  (cons (bit-and-not key modifier)
                        (cons (unmodifiers modifier) modifiers))
                  k)) k (keys unmodifiers))]
    (reverse (cons (char (first keys)) (rest keys)))))

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

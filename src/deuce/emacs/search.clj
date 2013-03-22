(ns deuce.emacs.search
  (:use [deuce.emacs-lisp :only (defun defvar) :as el])
  (:require [clojure.core :as c]
            [clojure.string :as s]
            [deuce.emacs.buffer :as buffer]
            [deuce.emacs.data :as data]
            [deuce.emacs.editfns :as editfns]
            [deuce.emacs-lisp.cons :as cons]
            [taoensso.timbre :as timbre])
  (:refer-clojure :exclude [])
  (:import [java.util.regex Pattern PatternSyntaxException]))

(defvar inhibit-changing-match-data nil
  "Internal use only.
  If non-nil, the primitive searching and matching functions
  such as `looking-at', `string-match', `re-search-forward', etc.,
  do not set the match data.  The proper way to use this variable
  is to bind it with `let' around a small expression.")

(defvar search-spaces-regexp nil
  "Regexp to substitute for bunches of spaces in regexp search.
  Some commands use this for user-specified regexps.
  Spaces that occur inside character classes or repetition operators
  or other such regexp constructs are not replaced with this.
  A value of nil (which is the normal value) means treat spaces literally.")

(declare string-match re-search-forward regexp-quote
         match-end match-data set-match-data)

(def ^:private current-match-data (atom nil))

(defun word-search-regexp (string &optional lax)
  "Return a regexp which matches words, ignoring punctuation.
  Given STRING, a string of words separated by word delimiters,
  compute a regexp that matches those exact words separated by
  arbitrary punctuation.  If LAX is non-nil, the end of the string
  need not match a word boundary unless it ends in whitespace.

  Used in `word-search-forward', `word-search-backward',
  `word-search-forward-lax', `word-search-backward-lax'."
  (str "\\b" (s/replace (s/trim string) #"\W+" "\\\\W\\\\W*")
       (when (or (not lax) (re-find #"\s$" string)) "\\b")))

(defun search-forward (string &optional bound noerror count)
  "Search forward from point for STRING.
  Set point to the end of the occurrence found, and return point.
  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend after that position.  A value of nil is
    equivalent to (point-max).
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument COUNT, if non-nil, means to search for COUNT
   successive occurrences.  If COUNT is negative, search backward,
   instead of forward, for -COUNT occurrences.

  Search case-sensitivity is determined by the value of the variable
  `case-fold-search', which see.

  See also the functions `match-beginning', `match-end' and `replace-match'."
  (re-search-forward (regexp-quote string) bound noerror count))

(defun re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for match for regular expression REGEXP.
  Set point to the beginning of the match, and return point.
  The match found is the one starting last in the buffer
  and yet ending before the origin of the search.
  An optional second argument bounds the search; it is a buffer position.
  The match found must start at or after that position.
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences.

  Search case-sensitivity is determined by the value of the variable
  `case-fold-search', which see.

  See also the functions `match-beginning', `match-end', `match-string',
  and `replace-match'."
  (let [point (editfns/point)
        count (el/check-type 'integerp (or count 1))]
    (cond
     (zero? count) point
     (neg? count) (re-search-forward regexp bound noerror (- count))
     :else
     (let [bound (el/check-type 'integerp (or bound 1))]
       (loop [offset (editfns/goto-char bound)
              matches []]
         (if (string-match (el/check-type 'stringp regexp)
                           (subs (editfns/buffer-string) 0 (dec point))
                           (dec offset))
           (when (< (editfns/goto-char (match-end 0)) point)
             (recur (editfns/point) (conj matches (match-data))))
           (if-let [match (nth (reverse matches) (dec count) nil)]
             (do (set-match-data match)
                 (editfns/goto-char (inc (first match))))
             (do (editfns/goto-char (if-not (contains? #{nil true} noerror)
                                      bound
                                      point))
                 (when-not noerror
                   (el/throw* 'search-failed (format "Search failed: %s" regexp)))))))))))

(defun set-match-data (list &optional reseat)
  "Set internal data on last search match from elements of LIST.
  LIST should have been created by calling `match-data' previously.

  If optional arg RESEAT is non-nil, make markers on LIST point nowhere."
  (reset! current-match-data list))

(defun word-search-forward-lax (string &optional bound noerror count)
  "Search forward from point for STRING, ignoring differences in punctuation.
  Set point to the end of the occurrence found, and return point.

  Unlike `word-search-forward', the end of STRING need not match a word
  boundary, unless STRING ends in whitespace.

  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend after that position.
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences.

  Relies on the function `word-search-regexp' to convert a sequence
  of words in STRING to a regexp used to search words without regard
  to punctuation."
  (re-search-forward (word-search-regexp string true) bound noerror count))

(defun word-search-backward (string &optional bound noerror count)
  "Search backward from point for STRING, ignoring differences in punctuation.
  Set point to the beginning of the occurrence found, and return point.
  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend before that position.
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences.

  Relies on the function `word-search-regexp' to convert a sequence
  of words in STRING to a regexp used to search words without regard
  to punctuation."
  (re-search-backward (word-search-regexp string) bound noerror count))

(defun posix-search-forward (regexp &optional bound noerror count)
  "Search forward from point for regular expression REGEXP.
  Find the longest match in accord with Posix regular expression rules.
  Set point to the end of the occurrence found, and return point.
  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend after that position.
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences.

  Search case-sensitivity is determined by the value of the variable
  `case-fold-search', which see.

  See also the functions `match-beginning', `match-end', `match-string',
  and `replace-match'."
  (search-forward regexp bound noerror count))

(defun word-search-forward (string &optional bound noerror count)
  "Search forward from point for STRING, ignoring differences in punctuation.
  Set point to the end of the occurrence found, and return point.
  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend after that position.
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences.

  Relies on the function `word-search-regexp' to convert a sequence
  of words in STRING to a regexp used to search words without regard
  to punctuation."
  (re-search-forward (word-search-regexp string) bound noerror count))

(defun word-search-backward-lax (string &optional bound noerror count)
  "Search backward from point for STRING, ignoring differences in punctuation.
  Set point to the beginning of the occurrence found, and return point.

  Unlike `word-search-backward', the end of STRING need not match a word
  boundary, unless STRING ends in whitespace.

  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend before that position.
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences.

  Relies on the function `word-search-regexp' to convert a sequence
  of words in STRING to a regexp used to search words without regard
  to punctuation."
  (re-search-backward (word-search-regexp string true) bound noerror count))

(defun looking-at (regexp)
  "Return t if text after point matches regular expression REGEXP.
  This function modifies the match data that `match-beginning',
  `match-end' and `match-data' access; save and restore the match
  data if you want to preserve them."
  (when (string-match (str "^" (el/check-type 'stringp regexp))
                      (editfns/buffer-string)
                      (dec (editfns/point)))
    true))

(defun re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for regular expression REGEXP.
  Set point to the end of the occurrence found, and return point.
  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend after that position.
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences.

  Search case-sensitivity is determined by the value of the variable
  `case-fold-search', which see.

  See also the functions `match-beginning', `match-end', `match-string',
  and `replace-match'."
  (let [point (editfns/point)
        count (el/check-type 'integerp (or count 1))]
    (cond
     (zero? count) point
     (neg? count) (re-search-backward regexp bound noerror (- count))
     :else
     (let [bound (el/check-type 'integerp (or bound (editfns/buffer-size)))]
       (loop [count (dec count)]
         (if (string-match (el/check-type 'stringp regexp)
                           (subs (editfns/buffer-string) 0 bound)
                           (dec (editfns/point)))
           (do (editfns/goto-char (inc (match-end 0)))
               (if (zero? count)
                 (editfns/point)
                 (recur (dec count))))
           (do (editfns/goto-char (if-not (contains? #{nil true} noerror)
                                    bound
                                    point))
               (when-not noerror
                 (el/throw* 'search-failed (format "Search failed: %s" regexp))))))))))

(defun posix-string-match (regexp string &optional start)
  "Return index of start of first match for REGEXP in STRING, or nil.
  Find the longest match, in accord with Posix regular expression rules.
  Case is ignored if `case-fold-search' is non-nil in the current buffer.
  If third arg START is non-nil, start search at that index in STRING.
  For index of first char beyond the match, do (match-end 0).
  `match-end' and `match-beginning' also give indices of substrings
  matched by parenthesis constructs in the pattern."
  (string-match regexp string start))

(defn ^:private emacs-regex-to-java [regexp]
  (-> regexp
      (s/replace #"^\\\`" "^")
      (s/replace #"\[(]?.*?)]"
                 (fn [x]
                   (str "[" (s/replace
                             (s/replace (x 1) "\\" "\\\\")
                             "[" "\\[")
                        "]")))
      (s/replace "\\(" "(")
      (s/replace "\\)" ")")))

(defun string-match (regexp string &optional start)
  "Return index of start of first match for REGEXP in STRING, or nil.
  Matching ignores case if `case-fold-search' is non-nil.
  If third arg START is non-nil, start search at that index in STRING.
  For index of first char beyond the match, do (match-end 0).
  `match-end' and `match-beginning' also give indices of substrings
  matched by parenthesis constructs in the pattern.

  You can use the function `match-string' to extract the substrings
  matched by the parenthesis constructions in REGEXP."
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Special.html
  (el/check-type 'stringp regexp)
  (el/check-type 'stringp string)
  (let [pattern (emacs-regex-to-java regexp)]
    (let [offset (el/check-type 'integerp (or start 0))
          ignore-case? (data/symbol-value 'case-fold-search)
          m (re-matcher (re-pattern (str (if ignore-case? "(?iu)" "") pattern))
                        (subs string offset))
          inhibit? (data/symbol-value 'inhibit-changing-match-data)]
      (if (re-find m)
        (let [m (cons/maybe-seq
                 (map (partial + offset)
                      (mapcat #(vector (.start m %) (.end m %))
                              (range (inc (.groupCount m))))))]
          (when-not inhibit?
            (reset! current-match-data m))
          (first m))
        (when-not inhibit?
          (reset! current-match-data nil))))))

(defun posix-looking-at (regexp)
  "Return t if text after point matches regular expression REGEXP.
  Find the longest match, in accord with Posix regular expression rules.
  This function modifies the match data that `match-beginning',
  `match-end' and `match-data' access; save and restore the match
  data if you want to preserve them."
  (looking-at regexp))

(defun match-data (&optional integers reuse reseat)
  "Return a list containing all info on what the last search matched.
  Element 2N is `(match-beginning N)'; element 2N + 1 is `(match-end N)'.
  All the elements are markers or nil (nil if the Nth pair didn't match)
  if the last match was on a buffer; integers or nil if a string was matched.
  Use `set-match-data' to reinstate the data in this list.

  If INTEGERS (the optional first argument) is non-nil, always use
  integers (rather than markers) to represent buffer positions.  In
  this case, and if the last match was in a buffer, the buffer will get
  stored as one additional element at the end of the list.

  If REUSE is a list, reuse it as part of the value.  If REUSE is long
  enough to hold all the values, and if INTEGERS is non-nil, no consing
  is done.

  If optional third arg RESEAT is non-nil, any previous markers on the
  REUSE list will be modified to point to nowhere.

  Return value is undefined if the last search failed."
  @current-match-data)

(defun replace-match (newtext &optional fixedcase literal string subexp)
  "Replace text matched by last search with NEWTEXT.
  Leave point at the end of the replacement text.

  If second arg FIXEDCASE is non-nil, do not alter case of replacement text.
  Otherwise maybe capitalize the whole text, or maybe just word initials,
  based on the replaced text.
  If the replaced text has only capital letters
  and has at least one multiletter word, convert NEWTEXT to all caps.
  Otherwise if all words are capitalized in the replaced text,
  capitalize each word in NEWTEXT.

  If third arg LITERAL is non-nil, insert NEWTEXT literally.
  Otherwise treat `\\' as special:
    `\\&' in NEWTEXT means substitute original matched text.
    `\\N' means substitute what matched the Nth `\\(...\\)'.
         If Nth parens didn't match, substitute nothing.
    `\\\\' means insert one `\\'.
  Case conversion does not apply to these substitutions.

  FIXEDCASE and LITERAL are optional arguments.

  The optional fourth argument STRING can be a string to modify.
  This is meaningful when the previous match was done against STRING,
  using `string-match'.  When used this way, `replace-match'
  creates and returns a new string made by copying STRING and replacing
  the part of STRING that was matched.

  The optional fifth argument SUBEXP specifies a subexpression;
  it says to replace just that subexpression with NEWTEXT,
  rather than replacing the entire matched text.
  This is, in a vague sense, the inverse of using `\\N' in NEWTEXT;
  `\\N' copies subexp N into NEWTEXT, but using N as SUBEXP puts
  NEWTEXT in place of subexp N.
  This is useful only after a regular expression search or match,
  since only regular expressions have distinguished subexpressions."
  )

(defun match-beginning (subexp)
  "Return position of start of text matched by last search.
  SUBEXP, a number, specifies which parenthesized expression in the last
    regexp.
  Value is nil if SUBEXPth pair didn't match, or there were less than
    SUBEXP pairs.
  Zero means the entire text matched by the whole regexp or whole string."
  (when @current-match-data
    (nth @current-match-data (* subexp 2) nil)))

(defun search-backward (string &optional bound noerror count)
  "Search backward from point for STRING.
  Set point to the beginning of the occurrence found, and return point.
  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend before that position.
  Optional third argument, if t, means if fail just return nil (no error).
   If not nil and not t, position at limit of search and return nil.
  Optional fourth argument COUNT, if non-nil, means to search for COUNT
   successive occurrences.  If COUNT is negative, search forward,
   instead of backward, for -COUNT occurrences.

  Search case-sensitivity is determined by the value of the variable
  `case-fold-search', which see.

  See also the functions `match-beginning', `match-end' and `replace-match'."
  (re-search-backward (regexp-quote string) bound noerror count))

(defun match-end (subexp)
  "Return position of end of text matched by last search.
  SUBEXP, a number, specifies which parenthesized expression in the last
    regexp.
  Value is nil if SUBEXPth pair didn't match, or there were less than
    SUBEXP pairs.
  Zero means the entire text matched by the whole regexp or whole string."
  (when @current-match-data
    (nth @current-match-data (inc (* subexp 2)) nil)))

(defun posix-search-backward (regexp &optional bound noerror count)
  "Search backward from point for match for regular expression REGEXP.
  Find the longest match in accord with Posix regular expression rules.
  Set point to the beginning of the match, and return point.
  The match found is the one starting last in the buffer
  and yet ending before the origin of the search.
  An optional second argument bounds the search; it is a buffer position.
  The match found must start at or after that position.
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences.

  Search case-sensitivity is determined by the value of the variable
  `case-fold-search', which see.

  See also the functions `match-beginning', `match-end', `match-string',
  and `replace-match'."
  (search-backward regexp bound noerror count))

(defun regexp-quote (string)
  "Return a regexp string which matches exactly STRING and nothing else."
  (let [slash (str (gensym "SLASH"))]
    (s/replace
     (reduce #(s/replace %1 (str %2) (str "\\" %2)) (s/replace string "\\" slash)
             "[*.?+^$")
     slash "\\\\")))

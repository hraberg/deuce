(ns
 deuce.emacs.search
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun search-forward (string &optional bound noerror count)
  "Search forward from point for STRING.
  Set point to the end of the occurrence found, and return point.
  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend after that position.  A value of nil is
    equivalent to (point-max).
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences."
  )

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
  See also the functions `match-beginning', `match-end', `match-string',
  and `replace-match'."
  )

(defun set-match-data (list &optional reseat)
  "Set internal data on last search match from elements of LIST.
  LIST should have been created by calling `match-data' previously."
  )

(defun word-search-forward-lax (string &optional bound noerror count)
  "Search forward from point for STRING, ignoring differences in punctuation.
  Set point to the end of the occurrence found, and return point."
  )

(defun word-search-backward (string &optional bound noerror count)
  "Search backward from point for STRING, ignoring differences in punctuation.
  Set point to the beginning of the occurrence found, and return point.
  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend before that position.
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences."
  )

(defun posix-search-forward (regexp &optional bound noerror count)
  "Search forward from point for regular expression REGEXP.
  Find the longest match in accord with Posix regular expression rules.
  Set point to the end of the occurrence found, and return point.
  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend after that position.
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences.
  See also the functions `match-beginning', `match-end', `match-string',
  and `replace-match'."
  )

(defun word-search-forward (string &optional bound noerror count)
  "Search forward from point for STRING, ignoring differences in punctuation.
  Set point to the end of the occurrence found, and return point.
  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend after that position.
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences."
  )

(defun word-search-backward-lax (string &optional bound noerror count)
  "Search backward from point for STRING, ignoring differences in punctuation.
  Set point to the beginning of the occurrence found, and return point."
  )

(defun looking-at (regexp)
  "Return t if text after point matches regular expression REGEXP.
  This function modifies the match data that `match-beginning',
  `match-end' and `match-data' access; save and restore the match
  data if you want to preserve them."
  )

(defun re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for regular expression REGEXP.
  Set point to the end of the occurrence found, and return point.
  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend after that position.
  Optional third argument, if t, means if fail just return nil (no error).
    If not nil and not t, move to limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences.
  See also the functions `match-beginning', `match-end', `match-string',
  and `replace-match'."
  )

(defun posix-string-match (regexp string &optional start)
  "Return index of start of first match for REGEXP in STRING, or nil.
  Find the longest match, in accord with Posix regular expression rules.
  Case is ignored if `case-fold-search' is non-nil in the current buffer.
  If third arg START is non-nil, start search at that index in STRING.
  For index of first char beyond the match, do (match-end 0).
  `match-end' and `match-beginning' also give indices of substrings
  matched by parenthesis constructs in the pattern."
  )

(defun string-match (regexp string &optional start)
  "Return index of start of first match for REGEXP in STRING, or nil.
  Matching ignores case if `case-fold-search' is non-nil.
  If third arg START is non-nil, start search at that index in STRING.
  For index of first char beyond the match, do (match-end 0).
  `match-end' and `match-beginning' also give indices of substrings
  matched by parenthesis constructs in the pattern."
  )

(defun posix-looking-at (regexp)
  "Return t if text after point matches regular expression REGEXP.
  Find the longest match, in accord with Posix regular expression rules.
  This function modifies the match data that `match-beginning',
  `match-end' and `match-data' access; save and restore the match
  data if you want to preserve them."
  )

(defun match-data (&optional integers reuse reseat)
  "Return a list containing all info on what the last search matched.
  Element 2N is `(match-beginning N)'; element 2N + 1 is `(match-end N)'.
  All the elements are markers or nil (nil if the Nth pair didn't match)
  if the last match was on a buffer; integers or nil if a string was matched.
  Use `set-match-data' to reinstate the data in this list."
  )

(defun replace-match (newtext &optional fixedcase literal string subexp)
  "Replace text matched by last search with NEWTEXT.
  Leave point at the end of the replacement text."
  )

(defun match-beginning (subexp)
  "Return position of start of text matched by last search.
  SUBEXP, a number, specifies which parenthesized expression in the last
    regexp.
  Value is nil if SUBEXPth pair didn't match, or there were less than
    SUBEXP pairs.
  Zero means the entire text matched by the whole regexp or whole string."
  )

(defun search-backward (string &optional bound noerror count)
  "Search backward from point for STRING.
  Set point to the beginning of the occurrence found, and return point.
  An optional second argument bounds the search; it is a buffer position.
  The match found must not extend before that position.
  Optional third argument, if t, means if fail just return nil (no error).
   If not nil and not t, position at limit of search and return nil.
  Optional fourth argument is repeat count--search for successive occurrences."
  )

(defun match-end (subexp)
  "Return position of end of text matched by last search.
  SUBEXP, a number, specifies which parenthesized expression in the last
    regexp.
  Value is nil if SUBEXPth pair didn't match, or there were less than
    SUBEXP pairs.
  Zero means the entire text matched by the whole regexp or whole string."
  )

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
  See also the functions `match-beginning', `match-end', `match-string',
  and `replace-match'."
  )

(defun regexp-quote (string)
  "Return a regexp string which matches exactly STRING and nothing else."
  )

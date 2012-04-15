(ns emacs.search (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun set-match-data (list &optional reseat)
  "Set internal data on last search match from elements of LIST.\nLIST should have been created by calling `match-data' previously."
  )

(defun looking-at (regexp)
  "Return t if text after point matches regular expression REGEXP.\nThis function modifies the match data that `match-beginning',\n`match-end' and `match-data' access; save and restore the match\ndata if you want to preserve them.set-safe-terminal-coding-system-internal is a built-in function in `C\nsource code'."
  )

(defun posix-string-match (regexp string &optional start)
  "Return index of start of first match for REGEXP in STRING, or nil.\nFind the longest match, in accord with Posix regular expression rules.\nCase is ignored if `case-fold-search' is non-nil in the current buffer.\nIf third arg START is non-nil, start search at that index in STRING.\nFor index of first char beyond the match, do (match-end 0).\n`match-end' and `match-beginning' also give indices of substrings\n"
  )

(defun string-match (regexp string &optional start)
  "Return index of start of first match for REGEXP in STRING, or nil.\nMatching ignores case if `case-fold-search' is non-nil.\nIf third arg START is non-nil, start search at that index in STRING.\nFor index of first char beyond the match, do (match-end 0).\n`match-end' and `match-beginning' also give indices of substrings\nmatched by parenthesis constructs in the pattern."
  )

(defun posix-looking-at (regexp)
  "Return t if text after point matches regular expression REGEXP.\nFind the longest match, in accord with Posix regular expression rules.\nThis function modifies the match data that `match-beginning',\n`match-end' and `match-data' access; save and restore the match\n"
  )

(defun match-data (&optional integers reuse reseat)
  "Return a list containing all info on what the last search matched.\nElement 2N is `(match-beginning N)'; element 2N + 1 is `(match-end N)'.\nAll the elements are markers or nil (nil if the Nth pair didn't match)\nif the last match was on a buffer; integers or nil if a string was matched.\nUse `set-match-data' to reinstate the data in this list."
  )

(defun replace-match (newtext &optional fixedcase literal string subexp)
  "Replace text matched by last search with NEWTEXT.\nLeave point at the end of the replacement text."
  )

(defun match-beginning (subexp)
  "Return position of start of text matched by last search.\nSUBEXP, a number, specifies which parenthesized expression in the last\n  regexp.\nValue is nil if SUBEXPth pair didn't match, or there were less than\n  SUBEXP pairs.\n"
  )

(defun match-end (subexp)
  "Return position of end of text matched by last search.\nSUBEXP, a number, specifies which parenthesized expression in the last\n  regexp.\nValue is nil if SUBEXPth pair didn't match, or there were less than\n  SUBEXP pairs.\n"
  )

(defun regexp-quote (string)
  "Return a regexp string which matches exactly STRING and nothing else.self-insert-command is an interactive built-in function in `C source\ncode'."
  )

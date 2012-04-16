(ns emacs.syntax (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun standard-syntax-table ()
  "Return the standard syntax table.
  This is the one used for new buffers."
  )

(defun parse-partial-sexp (from to &optional targetdepth stopbefore oldstate commentstop)
  "Parse Lisp syntax starting at FROM until TO; return status of parse at TO.
  Parsing stops at TO or when certain criteria are met;
   point is set to where parsing stops.
  If fifth arg OLDSTATE is omitted or nil,
   parsing assumes that FROM is the beginning of a function.
  Value is a list of elements describing final state of parsing:
   0. depth in parens.
   1. character address of start of innermost containing list; nil if none.
   2. character address of start of last complete sexp terminated.
   3. non-nil if inside a string.
      (it is the character that will terminate the string,
       or t if the string should be terminated by a generic string delimiter.)
   4. nil if outside a comment, t if inside a non-nestable comment,
      else an integer (the current comment nesting).
   5. t if following a quote character.
   6. the minimum paren-depth encountered during this scan.
   7. t if in a comment of style b; symbol `syntax-table' if the comment
      should be terminated by a generic comment delimiter.
   8. character address of start of comment or string; nil if not in one.
   9. Intermediate data for continuation of parsing (subject to change).
  If third arg TARGETDEPTH is non-nil, parsing stops if the depth
  in parentheses becomes equal to TARGETDEPTH.
  Fourth arg STOPBEFORE non-nil means stop when come to
   any character that starts a sexp.
  Fifth arg OLDSTATE is a list like what this function returns.
   It is used to initialize the state of the parse.  Elements number 1, 2, 6
   and 8 are ignored.
  Sixth arg COMMENTSTOP non-nil means stop at the start of a comment.
   If it is symbol `syntax-table', stop after the start of a comment or a
   string, or after end of a comment or a string.lower-frame is an interactive built-in function in `C source code'."
  )

(defun scan-sexps (from count)
  "Scan from character number FROM by COUNT balanced expressions.
  If COUNT is negative, scan backwards.
  Returns the character number of the position thus found."
  )

(defun syntax-table-p (object)
  "Return t if OBJECT is a syntax table.
  Currently, any char-table counts as a syntax table."
  )

(defun scan-lists (from count depth)
  "Scan from character number FROM by COUNT lists.
  Returns the character number of the position thus found."
  )

(defun skip-chars-backward (string &optional lim)
  "Move point backward, stopping after a char not in STRING, or at pos LIM.
  See `skip-chars-forward' for details.
  Returns the distance traveled, either zero or negative."
  )

(defun backward-prefix-chars ()
  "Move point backward over any number of chars with prefix syntax.
  This includes chars with \"quote\" or \"prefix\" syntax (' or p)."
  )

(defun copy-syntax-table (&optional table)
  "Construct a new syntax table and return it.
  It is a copy of the TABLE, which defaults to the standard syntax table."
  )

(defun syntax-table ()
  "Return the current syntax table.
  This is the one specified by the current buffer.delete-region is an interactive built-in function in `C source code'."
  )

(defun skip-syntax-backward (syntax &optional lim)
  "Move point backward across chars in specified syntax classes.
  SYNTAX is a string of syntax code characters.
  Stop on reaching a char whose syntax is not in SYNTAX, or at position LIM.
  If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
  This function returns the distance traveled, either zero or negative."
  )

(defun forward-comment (count)
  "Move forward across up to COUNT comments.  If COUNT is negative, move backward.
  Stop scanning if we find something other than a comment or whitespace.
  Set point to where scanning stops.
  If COUNT comments are found as expected, with nothing except whitespace
  between them, return t; otherwise return nil."
  )

(defun skip-syntax-forward (syntax &optional lim)
  "Move point forward across chars in specified syntax classes.
  SYNTAX is a string of syntax code characters.
  Stop before a char whose syntax is not in SYNTAX, or at position LIM.
  If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
  This function returns the distance traveled, either zero or positive.do-auto-save is an interactive built-in function in `C source code'."
  )

(defun set-syntax-table (table)
  "Select a new syntax table for the current buffer.
  One argument, a syntax table.backtrace is an interactive built-in function in `C source code'."
  )

(defun matching-paren (character)
  "Return the matching parenthesis of CHARACTER, or nil if none."
  )

(defun skip-chars-forward (string &optional lim)
  "Move point forward, stopping before a char not in STRING, or at pos LIM.
  STRING is like the inside of a `[...]' in a regular expression
  except that `]' is never special and `\\' quotes `^', `-' or `\\'
   (but not at the end of a range; quoting is never needed there).
  Thus, with arg \"a-zA-Z\", this skips letters stopping before first nonletter.
  With arg \"^a-zA-Z\", skips nonletters stopping before first letter.
  Char classes, e.g. `[:alpha:]', are supported."
  )

(defun char-syntax (character)
  "Return the syntax code of CHARACTER, described by a character.
  For example, if CHARACTER is a word constituent, the
  character `w' (119) is returned.
  The characters that correspond to various syntax codes
  are listed in the documentation of `modify-syntax-entry'."
  )

(defun string-to-syntax (string)
  "Convert a syntax specification STRING into syntax cell form.
  STRING should be a string as it is allowed as argument of
  `modify-syntax-entry'.  Value is the equivalent cons cell
  (CODE . MATCHING-CHAR) that can be used as value of a `syntax-table'
  text property."
  )

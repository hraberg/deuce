(ns emacs.syntax (use [deuce.core]) (:refer-clojure :only []))

(defun standard-syntax-table ()
  "Return the standard syntax table.\n"
  )

(defun parse-partial-sexp (from to &optional targetdepth stopbefore oldstate commentstop)
  "Parse Lisp syntax starting at FROM until TO; return status of parse at TO.\nParsing stops at TO or when certain criteria are met;\n point is set to where parsing stops.\nIf fifth arg OLDSTATE is omitted or nil,\n parsing assumes that FROM is the beginning of a function.\nValue is a list of elements describing final state of parsing:\n 0. depth in parens.\n 1. character address of start of innermost containing list; nil if none.\n 2. character address of start of last complete sexp terminated.\n 3. non-nil if inside a string.\n    (it is the character that will terminate the string,\n     or t if the string should be terminated by a generic string delimiter.)\n 4. nil if outside a comment, t if inside a non-nestable comment,\n    else an integer (the current comment nesting).\n 5. t if following a quote character.\n 6. the minimum paren-depth encountered during this scan.\n 7. t if in a comment of style b; symbol `syntax-table' if the comment\n    should be terminated by a generic comment delimiter.\n 8. character address of start of comment or string; nil if not in one.\n 9. Intermediate data for continuation of parsing (subject to change).\nIf third arg TARGETDEPTH is non-nil, parsing stops if the depth\nin parentheses becomes equal to TARGETDEPTH.\nFourth arg STOPBEFORE non-nil means stop when come to\n any character that starts a sexp.\nFifth arg OLDSTATE is a list like what this function returns.\n It is used to initialize the state of the parse.  Elements number 1, 2, 6\n and 8 are ignored.\nSixth arg COMMENTSTOP non-nil means stop at the start of a comment.\n If it is symbol `syntax-table', stop after the start of a comment or a\n string, or after end of a comment or a string.lower-frame is an interactive built-in function in `C source code'."
  )

(defun scan-sexps (from count)
  "Scan from character number FROM by COUNT balanced expressions.\nIf COUNT is negative, scan backwards.\nReturns the character number of the position thus found."
  )

(defun syntax-table-p (object)
  "Return t if OBJECT is a syntax table.\n"
  )

(defun scan-lists (from count depth)
  "Scan from character number FROM by COUNT lists.\nReturns the character number of the position thus found."
  )

(defun skip-chars-backward (string &optional lim)
  "Move point backward, stopping after a char not in STRING, or at pos LIM.\nSee `skip-chars-forward' for details.\n"
  )

(defun backward-prefix-chars ()
  "Move point backward over any number of chars with prefix syntax.\n"
  )

(defun copy-syntax-table (&optional table)
  "Construct a new syntax table and return it.\n"
  )

(defun syntax-table ()
  "Return the current syntax table.\nThis is the one specified by the current buffer.delete-region is an interactive built-in function in `C source code'."
  )

(defun skip-syntax-backward (syntax &optional lim)
  "Move point backward across chars in specified syntax classes.\nSYNTAX is a string of syntax code characters.\nStop on reaching a char whose syntax is not in SYNTAX, or at position LIM.\nIf SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.\n"
  )

(defun forward-comment (count)
  "Move forward across up to COUNT comments.  If COUNT is negative, move backward.\nStop scanning if we find something other than a comment or whitespace.\nSet point to where scanning stops.\nIf COUNT comments are found as expected, with nothing except whitespace\n"
  )

(defun skip-syntax-forward (syntax &optional lim)
  "Move point forward across chars in specified syntax classes.\nSYNTAX is a string of syntax code characters.\nStop before a char whose syntax is not in SYNTAX, or at position LIM.\nIf SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.\nThis function returns the distance traveled, either zero or positive.do-auto-save is an interactive built-in function in `C source code'."
  )

(defun set-syntax-table (table)
  "Select a new syntax table for the current buffer.\nOne argument, a syntax table.backtrace is an interactive built-in function in `C source code'."
  )

(defun matching-paren (character)
  )

(defun skip-chars-forward (string &optional lim)
  "Move point forward, stopping before a char not in STRING, or at pos LIM.\nSTRING is like the inside of a `[...]' in a regular expression\nexcept that `]' is never special and `\\' quotes `^', `-' or `\\'\n (but not at the end of a range; quoting is never needed there).\nThus, with arg \"a-zA-Z\", this skips letters stopping before first nonletter.\nWith arg \"^a-zA-Z\", skips nonletters stopping before first letter.\nChar classes, e.g. `[:alpha:]', are supported."
  )

(defun char-syntax (character)
  "Return the syntax code of CHARACTER, described by a character.\nFor example, if CHARACTER is a word constituent, the\ncharacter `w' (119) is returned.\nThe characters that correspond to various syntax codes\n"
  )

(defun string-to-syntax (string)
  "Convert a syntax specification STRING into syntax cell form.\nSTRING should be a string as it is allowed as argument of\n`modify-syntax-entry'.  Value is the equivalent cons cell\n(CODE . MATCHING-CHAR) that can be used as value of a `syntax-table'\n"
  )

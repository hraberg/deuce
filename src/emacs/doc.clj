(ns emacs.doc (use [deuce.core]) (:refer-clojure :only []))

(defun Snarf-documentation (filename)
  "Used during Emacs initialization to scan the `etc/DOC...' file.\nThis searches the `etc/DOC...' file for doc strings and\nrecords them in function and variable definitions.\nThe function takes one argument, FILENAME, a string;\nit specifies the file name (without a directory) of the DOC file.\nThat file is found in `../etc' now; later, when the dumped Emacs is run,\nthe same file name is found in the `doc-directory'.search-forward-regexp is an interactive built-in function in\n`subr.el'."
  )

(defun substitute-command-keys (string)
  "Substitute key descriptions for command names in STRING.\nSubstrings of the form \\[COMMAND] replaced by either: a keystroke\nsequence that will invoke COMMAND, or \"M-x COMMAND\" if COMMAND is not\non any keys.\nSubstrings of the form \\{MAPVAR} are replaced by summaries\n(made by `describe-bindings') of the value of MAPVAR, taken as a keymap.\nSubstrings of the form \\<MAPVAR> specify to use the value of MAPVAR\nas the keymap for future \\[COMMAND] substrings.\n\\= quotes the following character and is discarded;\nthus, \\=\\= puts \\= into the output, and \\=\\[ puts \\[ into the output."
  )

(defun documentation-property (symbol prop &optional raw)
  "Return the documentation string that is SYMBOL's PROP property.\nThird argument RAW omitted or nil means pass the result through\n`substitute-command-keys' if it is a string."
  )

(defun documentation (function &optional raw)
  "Return the documentation string of FUNCTION.\nUnless a non-nil second argument RAW is given, the\n"
  )

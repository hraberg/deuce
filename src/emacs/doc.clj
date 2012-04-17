(ns
 emacs.doc
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun Snarf-documentation (filename)
  "Used during Emacs initialization to scan the `etc/DOC...' file.
  This searches the `etc/DOC...' file for doc strings and
  records them in function and variable definitions.
  The function takes one argument, FILENAME, a string;
  it specifies the file name (without a directory) of the DOC file.
  That file is found in `../etc' now; later, when the dumped Emacs is run,
  the same file name is found in the `doc-directory'."
  )

(defun substitute-command-keys (string)
  "Substitute key descriptions for command names in STRING.
  Substrings of the form \\[COMMAND] replaced by either: a keystroke
  sequence that will invoke COMMAND, or \"M-x COMMAND\" if COMMAND is not
  on any keys.
  Substrings of the form \\{MAPVAR} are replaced by summaries
  (made by `describe-bindings') of the value of MAPVAR, taken as a keymap.
  Substrings of the form \\<MAPVAR> specify to use the value of MAPVAR
  as the keymap for future \\[COMMAND] substrings.
  \\= quotes the following character and is discarded;
  thus, \\=\\= puts \\= into the output, and \\=\\[ puts \\[ into the output."
  )

(defun documentation-property (symbol prop &optional raw)
  "Return the documentation string that is SYMBOL's PROP property.
  Third argument RAW omitted or nil means pass the result through
  `substitute-command-keys' if it is a string."
  )

(defun documentation (function &optional raw)
  "Return the documentation string of FUNCTION.
  Unless a non-nil second argument RAW is given, the
  string is passed through `substitute-command-keys'."
  )

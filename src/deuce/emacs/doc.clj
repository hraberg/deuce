(ns
 deuce.emacs.doc
 (:use [deuce.emacs-lisp :only (defun defvar)])
 (:refer-clojure :exclude []))

(defvar build-files nil
  "A list of files used to build this Emacs binary.")

(defvar internal-doc-file-name nil
  "Name of file containing documentation strings of built-in symbols.")

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
  Each substring of the form \\[COMMAND] is replaced by either a
  keystroke sequence that invokes COMMAND, or \"M-x COMMAND\" if COMMAND
  is not on any keys.
  
  Each substring of the form \\{MAPVAR} is replaced by a summary of
  the value of MAPVAR as a keymap.  This summary is similar to the one
  produced by `describe-bindings'.  The summary ends in two newlines
  (used by the helper function `help-make-xrefs' to find the end of the
  summary).
  
  Each substring of the form \\<MAPVAR> specifies the use of MAPVAR
  as the keymap for future \\[COMMAND] substrings.
  \\= quotes the following character and is discarded;
  thus, \\=\\= puts \\= into the output, and \\=\\[ puts \\[ into the output.
  
  Return the original STRING if no substitutions are made.
  Otherwise, return a new string, without any text properties."
  )

(defun documentation-property (symbol prop &optional raw)
  "Return the documentation string that is SYMBOL's PROP property.
  Third argument RAW omitted or nil means pass the result through
  `substitute-command-keys' if it is a string.
  
  This differs from `get' in that it can refer to strings stored in the
  `etc/DOC' file; and that it evaluates documentation properties that
  aren't strings."
  )

(defun documentation (function &optional raw)
  "Return the documentation string of FUNCTION.
  Unless a non-nil second argument RAW is given, the
  string is passed through `substitute-command-keys'."
  )

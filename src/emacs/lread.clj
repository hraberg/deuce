(ns emacs.lread (use [deuce.core]) (:refer-clojure :only []))

(defun read-event (&optional prompt inherit-input-method seconds)
  "Read an event object from the input stream.\nIf the optional argument PROMPT is non-nil, display that as a prompt.\nIf the optional argument INHERIT-INPUT-METHOD is non-nil and some\ninput method is turned on in the current buffer, that input method\nis used for reading a character.\nIf the optional argument SECONDS is non-nil, it should be a number\nspecifying the maximum number of seconds to wait for input.  If no\ninput arrives in that time, return nil.  SECONDS may be a\nfloating-point value.re-search-forward is an interactive built-in function in `C source\ncode'."
  )

(defun read-char-exclusive (&optional prompt inherit-input-method seconds)
  "Read a character from the command input (keyboard or macro).\nIt is returned as a number.  Non-character events are ignored.\nIf the character has modifiers, they are resolved and reflected to the\ncharacter code if possible (e.g. C-SPC -> 0)."
  )

(defun read (&optional stream)
  "Read one Lisp expression as text from STREAM, return as Lisp object.\nIf STREAM is nil, use the value of `standard-input' (which see).\nSTREAM or the value of `standard-input' may be:\n a buffer (read from point and advance it)\n a marker (read from where it points and advance it)\n a function (call it with no arguments for each character,\n     call it with a char as argument to push a char back)\n a string (takes text from string, starting at the beginning)\n t (read text line using minibuffer and use it, or read from\n    standard input in batch mode)."
  )

(defun read-char (&optional prompt inherit-input-method seconds)
  "Read a character from the command input (keyboard or macro).\nIt is returned as a number.\nIf the character has modifiers, they are resolved and reflected to the\ncharacter code if possible (e.g. C-SPC -> 0)."
  )

(defun read-from-string (string &optional start end)
  "Read one Lisp expression which is represented as text by STRING.\nReturns a cons: (OBJECT-READ . FINAL-STRING-INDEX).\nSTART and END optionally delimit a substring of STRING from which to read;\n"
  )

(defun intern (string &optional obarray)
  "Return the canonical symbol whose name is STRING.\nIf there is none, one is created by this function and returned.\nA second optional argument specifies the obarray to use;\n"
  )

(defun get-load-suffixes ()
  "Return the suffixes that `load' should try if a suffix is required.\n"
  )

(defun load (file &optional noerror nomessage nosuffix must-suffix)
  "Execute a file of Lisp code named FILE.\nFirst try FILE with `.elc' appended, then try with `.el',\nthen try FILE unmodified (the exact suffixes in the exact order are\ndetermined by `load-suffixes').  Environment variable references in\nFILE are replaced with their values by calling `substitute-in-file-name'.\nThis function searches the directories in `load-path'."
  )

(defun mapatoms (function &optional obarray)
  "Call FUNCTION on every symbol in OBARRAY.\n"
  )

(defun locate-file-internal (filename path &optional suffixes predicate)
  "Search for FILENAME through PATH.\nReturns the file's name in absolute form, or nil if not found.\nIf SUFFIXES is non-nil, it should be a list of suffixes to append to\nfile name when searching.\nIf non-nil, PREDICATE is used instead of `file-readable-p'.\nPREDICATE can also be an integer to pass to the access(2) function,\n"
  )

(defun unintern (name obarray)
  "Delete the symbol named NAME, if any, from OBARRAY.\nThe value is t if a symbol was found and deleted, nil otherwise.\nNAME may be a string or a symbol.  If it is a symbol, that symbol\nis deleted, if it belongs to OBARRAY--no other symbol is deleted.\n"
  )

(defun get-file-char ()
  "Don't use this yourself.defun is a special form in `C source code'."
  )

(defun intern-soft (name &optional obarray)
  "Return the canonical symbol named NAME, or nil if none exists.\nNAME may be a string or a symbol.  If it is a symbol, that exact\nsymbol is searched for.\nA second optional argument specifies the obarray to use;\nit defaults to the value of `obarray'.window-redisplay-end-trigger is a built-in function in `C source\ncode'."
  )

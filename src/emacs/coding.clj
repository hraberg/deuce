(ns emacs.coding (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun coding-system-base (coding-system)
  "Return the base of CODING-SYSTEM."
  )

(defun encode-big5-char (ch)
  "Encode the Big5 character CH to BIG5 coding system."
  )

(defun coding-system-eol-type (coding-system)
  "Return eol-type of CODING-SYSTEM.
  An eol-type is an integer 0, 1, 2, or a vector of coding systems."
  )

(defun coding-system-aliases (coding-system)
  )

(defun define-coding-system-alias (alias coding-system)
  )

(defun decode-big5-char (code)
  "Decode a Big5 character which has CODE in BIG5 coding system."
  )

(defun coding-system-plist (coding-system)
  )

(defun detect-coding-string (string &optional highest)
  "Detect coding system of the text in STRING.
  Return a list of possible coding systems ordered by priority.
  The coding systems to try and their priorities follows what
  the function `coding-system-priority-list' (which see) returns."
  )

(defun decode-sjis-char (code)
  "Decode a Japanese character which has CODE in shift_jis encoding."
  )

(defun unencodable-char-position (start end coding-system &optional count string)
  "Return position of first un-encodable character in a region.
  START and END specify the region and CODING-SYSTEM specifies the
  encoding to check.  Return nil if CODING-SYSTEM does encode the region."
  )

(defun read-coding-system (prompt &optional default-coding-system)
  "Read a coding system from the minibuffer, prompting with string PROMPT.
  If the user enters null input, return second argument DEFAULT-CODING-SYSTEM.
  Ignores case when completing coding systems (all Emacs coding systems
  are lower-case).scroll-right is an interactive built-in function in `C source code'."
  )

(defun check-coding-systems-region (start end coding-system-list)
  "Check if the region is encodable by coding systems."
  )

(defun encode-coding-string (string coding-system &optional nocopy buffer)
  "Encode STRING to CODING-SYSTEM, and return the result."
  )

(defun terminal-coding-system (&optional terminal)
  "Return coding system specified for terminal output on the given terminal.
  TERMINAL may be a terminal object, a frame, or nil for the selected"
  )

(defun coding-system-priority-list (&optional highestp)
  "Return a list of coding systems ordered by their priorities.
  The list contains a subset of coding systems; i.e. coding systems
  assigned to each coding category (see `coding-category-list')."
  )

(defun keyboard-coding-system (&optional terminal)
  )

(defun detect-coding-region (start end &optional highest)
  "Detect coding system of the text in the region between START and END.
  Return a list of possible coding systems ordered by priority.
  The coding systems to try and their priorities follows what
  the function `coding-system-priority-list' (which see) returns."
  )

(defun coding-system-put (coding-system prop val)
  )

(defun decode-coding-string (string coding-system &optional nocopy buffer)
  "Decode STRING which is encoded in CODING-SYSTEM, and return the result."
  )

(defun read-non-nil-coding-system (prompt)
  )

(defun encode-sjis-char (ch)
  "Encode a Japanese character CH to shift_jis encoding."
  )

(defun set-coding-system-priority (&rest coding-systems)
  "Assign higher priority to the coding systems given as arguments.
  If multiple coding systems belong to the same category,"
  )

(defun check-coding-system (coding-system)
  "Check validity of CODING-SYSTEM.
  If valid, return CODING-SYSTEM, else signal a `coding-system-error' error.
  It is valid if it is nil or a symbol defined as a coding system by the
  function `define-coding-system'.defvar is a special form in `C source code'."
  )

(defun coding-system-p (object)
  "Return t if OBJECT is nil or a coding-system.
  See the documentation of `define-coding-system' for information"
  )

(ns
 deuce.emacs.coding
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun coding-system-base (coding-system)
  "Return the base of CODING-SYSTEM.
  Any alias or subsidiary coding system is not a base coding system."
  )

(defun encode-big5-char (ch)
  "Encode the Big5 character CH to BIG5 coding system.
  Return the corresponding character code in Big5."
  )

(defun coding-system-eol-type (coding-system)
  "Return eol-type of CODING-SYSTEM.
  An eol-type is an integer 0, 1, 2, or a vector of coding systems."
  )

(defun coding-system-aliases (coding-system)
  "Return the list of aliases of CODING-SYSTEM."
  )

(defun set-safe-terminal-coding-system-internal (coding-system)
  "Internal use only."
  )

(defun define-coding-system-alias (alias coding-system)
  "Define ALIAS as an alias for CODING-SYSTEM."
  )

(defun decode-big5-char (code)
  "Decode a Big5 character which has CODE in BIG5 coding system.
  Return the corresponding character."
  )

(defun set-keyboard-coding-system-internal (coding-system &optional terminal)
  "Internal use only."
  )

(defun coding-system-plist (coding-system)
  "Return the property list of CODING-SYSTEM."
  )

(defun find-coding-systems-region-internal (start end &optional exclude)
  "Internal use only."
  )

(defun decode-coding-region (start end coding-system &optional destination)
  "Decode the current region from the specified coding system.
  When called from a program, takes four arguments:
  	START, END, CODING-SYSTEM, and DESTINATION.
  START and END are buffer positions."
  )

(defun detect-coding-string (string &optional highest)
  "Detect coding system of the text in STRING.
  Return a list of possible coding systems ordered by priority.
  The coding systems to try and their priorities follows what
  the function `coding-system-priority-list' (which see) returns."
  )

(defun encode-coding-region (start end coding-system &optional destination)
  "Encode the current region by specified coding system.
  When called from a program, takes four arguments:
          START, END, CODING-SYSTEM and DESTINATION.
  START and END are buffer positions."
  )

(defun decode-sjis-char (code)
  "Decode a Japanese character which has CODE in shift_jis encoding.
  Return the corresponding character."
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
  are lower-case)."
  )

(defun check-coding-systems-region (start end coding-system-list)
  "Check if the region is encodable by coding systems."
  )

(defun encode-coding-string (string coding-system &optional nocopy buffer)
  "Encode STRING to CODING-SYSTEM, and return the result."
  )

(defun find-operation-coding-system (operation &rest arguments)
  "Choose a coding system for an operation based on the target name.
  The value names a pair of coding systems: (DECODING-SYSTEM . ENCODING-SYSTEM).
  DECODING-SYSTEM is the coding system to use for decoding
  (in case OPERATION does decoding), and ENCODING-SYSTEM is the coding system
  for encoding (in case OPERATION does encoding)."
  )

(defun terminal-coding-system (&optional terminal)
  "Return coding system specified for terminal output on the given terminal.
  TERMINAL may be a terminal object, a frame, or nil for the selected
  frame's terminal device."
  )

(defun coding-system-priority-list (&optional highestp)
  "Return a list of coding systems ordered by their priorities.
  The list contains a subset of coding systems; i.e. coding systems
  assigned to each coding category (see `coding-category-list')."
  )

(defun keyboard-coding-system (&optional terminal)
  "Return coding system specified for decoding keyboard input."
  )

(defun detect-coding-region (start end &optional highest)
  "Detect coding system of the text in the region between START and END.
  Return a list of possible coding systems ordered by priority.
  The coding systems to try and their priorities follows what
  the function `coding-system-priority-list' (which see) returns."
  )

(defun define-coding-system-internal (&rest args)
  "For internal use only."
  )

(defun coding-system-put (coding-system prop val)
  "Change value in CODING-SYSTEM's property list PROP to VAL."
  )

(defun decode-coding-string (string coding-system &optional nocopy buffer)
  "Decode STRING which is encoded in CODING-SYSTEM, and return the result."
  )

(defun set-terminal-coding-system-internal (coding-system &optional terminal)
  "Internal use only."
  )

(defun read-non-nil-coding-system (prompt)
  "Read a coding system from the minibuffer, prompting with string PROMPT."
  )

(defun encode-sjis-char (ch)
  "Encode a Japanese character CH to shift_jis encoding.
  Return the corresponding code in SJIS."
  )

(defun set-coding-system-priority (&rest coding-systems)
  "Assign higher priority to the coding systems given as arguments.
  If multiple coding systems belong to the same category,
  all but the first one are ignored."
  )

(defun check-coding-system (coding-system)
  "Check validity of CODING-SYSTEM.
  If valid, return CODING-SYSTEM, else signal a `coding-system-error' error.
  It is valid if it is nil or a symbol defined as a coding system by the
  function `define-coding-system'."
  )

(defun coding-system-p (object)
  "Return t if OBJECT is nil or a coding-system.
  See the documentation of `define-coding-system' for information
  about coding-system objects."
  )

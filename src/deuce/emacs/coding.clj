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
  An eol-type is an integer 0, 1, 2, or a vector of coding systems.
  
  Integer values 0, 1, and 2 indicate a format of end-of-line; LF, CRLF,
  and CR respectively.
  
  A vector value indicates that a format of end-of-line should be
  detected automatically.  Nth element of the vector is the subsidiary
  coding system whose eol-type is N."
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
  START and END are buffer positions.
  
  Optional 4th arguments DESTINATION specifies where the decoded text goes.
  If nil, the region between START and END is replaced by the decoded text.
  If buffer, the decoded text is inserted in that buffer after point (point
  does not move).
  In those cases, the length of the decoded text is returned.
  If DESTINATION is t, the decoded text is returned.
  
  This function sets `last-coding-system-used' to the precise coding system
  used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
  not fully specified.)"
  )

(defun detect-coding-string (string &optional highest)
  "Detect coding system of the text in STRING.
  Return a list of possible coding systems ordered by priority.
  The coding systems to try and their priorities follows what
  the function `coding-system-priority-list' (which see) returns.
  
  If only ASCII characters are found (except for such ISO-2022 control
  characters as ESC), it returns a list of single element `undecided'
  or its subsidiary coding system according to a detected end-of-line
  format.
  
  If optional argument HIGHEST is non-nil, return the coding system of
  highest priority."
  )

(defun encode-coding-region (start end coding-system &optional destination)
  "Encode the current region by specified coding system.
  When called from a program, takes four arguments:
          START, END, CODING-SYSTEM and DESTINATION.
  START and END are buffer positions.
  
  Optional 4th arguments DESTINATION specifies where the encoded text goes.
  If nil, the region between START and END is replace by the encoded text.
  If buffer, the encoded text is inserted in that buffer after point (point
  does not move).
  In those cases, the length of the encoded text is returned.
  If DESTINATION is t, the encoded text is returned.
  
  This function sets `last-coding-system-used' to the precise coding system
  used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
  not fully specified.)"
  )

(defun decode-sjis-char (code)
  "Decode a Japanese character which has CODE in shift_jis encoding.
  Return the corresponding character."
  )

(defun unencodable-char-position (start end coding-system &optional count string)
  "Return position of first un-encodable character in a region.
  START and END specify the region and CODING-SYSTEM specifies the
  encoding to check.  Return nil if CODING-SYSTEM does encode the region.
  
  If optional 4th argument COUNT is non-nil, it specifies at most how
  many un-encodable characters to search.  In this case, the value is a
  list of positions.
  
  If optional 5th argument STRING is non-nil, it is a string to search
  for un-encodable characters.  In that case, START and END are indexes
  to the string."
  )

(defun read-coding-system (prompt &optional default-coding-system)
  "Read a coding system from the minibuffer, prompting with string PROMPT.
  If the user enters null input, return second argument DEFAULT-CODING-SYSTEM.
  Ignores case when completing coding systems (all Emacs coding systems
  are lower-case)."
  )

(defun check-coding-systems-region (start end coding-system-list)
  "Check if the region is encodable by coding systems.
  
  START and END are buffer positions specifying the region.
  CODING-SYSTEM-LIST is a list of coding systems to check.
  
  The value is an alist ((CODING-SYSTEM POS0 POS1 ...) ...), where
  CODING-SYSTEM is a member of CODING-SYSTEM-LIST and can't encode the
  whole region, POS0, POS1, ... are buffer positions where non-encodable
  characters are found.
  
  If all coding systems in CODING-SYSTEM-LIST can encode the region, the
  value is nil.
  
  START may be a string.  In that case, check if the string is
  encodable, and the value contains indices to the string instead of
  buffer positions.  END is ignored.
  
  If the current buffer (or START if it is a string) is unibyte, the value
  is nil."
  )

(defun encode-coding-string (string coding-system &optional nocopy buffer)
  "Encode STRING to CODING-SYSTEM, and return the result.
  
  Optional third arg NOCOPY non-nil means it is OK to return STRING
  itself if the encoding operation is trivial.
  
  Optional fourth arg BUFFER non-nil means that the encoded text is
  inserted in that buffer after point (point does not move).  In this
  case, the return value is the length of the encoded text.
  
  This function sets `last-coding-system-used' to the precise coding system
  used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
  not fully specified.)"
  )

(defun find-operation-coding-system (operation &rest arguments)
  "Choose a coding system for an operation based on the target name.
  The value names a pair of coding systems: (DECODING-SYSTEM . ENCODING-SYSTEM).
  DECODING-SYSTEM is the coding system to use for decoding
  (in case OPERATION does decoding), and ENCODING-SYSTEM is the coding system
  for encoding (in case OPERATION does encoding).
  
  The first argument OPERATION specifies an I/O primitive:
    For file I/O, `insert-file-contents' or `write-region'.
    For process I/O, `call-process', `call-process-region', or `start-process'.
    For network I/O, `open-network-stream'.
  
  The remaining arguments should be the same arguments that were passed
  to the primitive.  Depending on which primitive, one of those arguments
  is selected as the TARGET.  For example, if OPERATION does file I/O,
  whichever argument specifies the file name is TARGET.
  
  TARGET has a meaning which depends on OPERATION:
    For file I/O, TARGET is a file name (except for the special case below).
    For process I/O, TARGET is a process name.
    For network I/O, TARGET is a service name or a port number.
  
  This function looks up what is specified for TARGET in
  `file-coding-system-alist', `process-coding-system-alist',
  or `network-coding-system-alist' depending on OPERATION.
  They may specify a coding system, a cons of coding systems,
  or a function symbol to call.
  In the last case, we call the function with one argument,
  which is a list of all the arguments given to this function.
  If the function can't decide a coding system, it can return
  `undecided' so that the normal code-detection is performed.
  
  If OPERATION is `insert-file-contents', the argument corresponding to
  TARGET may be a cons (FILENAME . BUFFER).  In that case, FILENAME is a
  file name to look up, and BUFFER is a buffer that contains the file's
  contents (not yet decoded).  If `file-coding-system-alist' specifies a
  function to call for FILENAME, that function should examine the
  contents of BUFFER instead of reading the file."
  )

(defun terminal-coding-system (&optional terminal)
  "Return coding system specified for terminal output on the given terminal.
  TERMINAL may be a terminal object, a frame, or nil for the selected
  frame's terminal device."
  )

(defun coding-system-priority-list (&optional highestp)
  "Return a list of coding systems ordered by their priorities.
  The list contains a subset of coding systems; i.e. coding systems
  assigned to each coding category (see `coding-category-list').
  
  HIGHESTP non-nil means just return the highest priority one."
  )

(defun keyboard-coding-system (&optional terminal)
  "Return coding system specified for decoding keyboard input."
  )

(defun detect-coding-region (start end &optional highest)
  "Detect coding system of the text in the region between START and END.
  Return a list of possible coding systems ordered by priority.
  The coding systems to try and their priorities follows what
  the function `coding-system-priority-list' (which see) returns.
  
  If only ASCII characters are found (except for such ISO-2022 control
  characters as ESC), it returns a list of single element `undecided'
  or its subsidiary coding system according to a detected end-of-line
  format.
  
  If optional argument HIGHEST is non-nil, return the coding system of
  highest priority."
  )

(defun define-coding-system-internal (&rest args)
  "For internal use only."
  )

(defun coding-system-put (coding-system prop val)
  "Change value in CODING-SYSTEM's property list PROP to VAL."
  )

(defun decode-coding-string (string coding-system &optional nocopy buffer)
  "Decode STRING which is encoded in CODING-SYSTEM, and return the result.
  
  Optional third arg NOCOPY non-nil means it is OK to return STRING itself
  if the decoding operation is trivial.
  
  Optional fourth arg BUFFER non-nil means that the decoded text is
  inserted in that buffer after point (point does not move).  In this
  case, the return value is the length of the decoded text.
  
  This function sets `last-coding-system-used' to the precise coding system
  used (which may be different from CODING-SYSTEM if CODING-SYSTEM is
  not fully specified.)"
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

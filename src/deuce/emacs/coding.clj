(ns deuce.emacs.coding
  (:use [deuce.emacs-lisp :only (defun defvar) :as el])
  (:require [clojure.core :as c]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.charset :as charset]
            [deuce.emacs.fns :as fns]
            [deuce.emacs-lisp.globals :as globals])
  (:refer-clojure :exclude []))

(defvar inhibit-null-byte-detection nil
  "If non-nil, Emacs ignores null bytes on code detection.
  By default, Emacs treats it as binary data, and does not attempt to
  decode it.  The effect is as if you specified `no-conversion' for
  reading that text.

  Set this to non-nil when a regular text happens to include null bytes.
  Examples are Index nodes of Info files and null-byte delimited output
  from GNU Find and GNU Grep.  Emacs will then ignore the null bytes and
  decode text as usual.")

(defvar eol-mnemonic-undecided ":"
  "*String displayed in mode line when end-of-line format is not yet determined.

  You can customize this variable.")

(defvar latin-extra-code-table nil
  "Table of extra Latin codes in the range 128..159 (inclusive).
  This is a vector of length 256.
  If Nth element is non-nil, the existence of code N in a file
  (or output of subprocess) doesn't prevent it to be detected as
  a coding system of ISO 2022 variant which has a flag
  `accept-latin-extra-code' t (e.g. iso-latin-1) on reading a file
  or reading output of a subprocess.
  Only 128th through 159th elements have a meaning.")

(defvar last-coding-system-used nil
  "Coding system used in the latest file or process I/O.")

(defvar charset-revision-table nil
  "Alist of charsets vs revision numbers.
  While encoding, if a charset (car part of an element) is found,
  designate it with the escape sequence identifying revision (cdr part
  of the element).")

(defvar standard-translation-table-for-decode nil
  "Table for translating characters while decoding.")

(defvar inhibit-iso-escape-detection nil
  "If non-nil, Emacs ignores ISO-2022 escape sequences during code detection.

  When Emacs reads text, it tries to detect how the text is encoded.
  This code detection is sensitive to escape sequences.  If Emacs sees
  a valid ISO-2022 escape sequence, it assumes the text is encoded in one
  of the ISO2022 encodings, and decodes text by the corresponding coding
  system (e.g. `iso-2022-7bit').

  However, there may be a case that you want to read escape sequences in
  a file as is.  In such a case, you can set this variable to non-nil.
  Then the code detection will ignore any escape sequences, and no text is
  detected as encoded in some ISO-2022 encoding.  The result is that all
  escape sequences become visible in a buffer.

  The default value is nil, and it is strongly recommended not to change
  it.  That is because many Emacs Lisp source files that contain
  non-ASCII characters are encoded by the coding system `iso-2022-7bit'
  in Emacs's distribution, and they won't be decoded correctly on
  reading if you suppress escape sequence detection.

  The other way to read escape sequences in a file without decoding is
  to explicitly specify some coding system that doesn't use ISO-2022
  escape sequence (e.g `latin-1') on reading by C-x RET c.")

(defvar coding-system-list nil
  "List of coding systems.

  Do not alter the value of this variable manually.  This variable should be
  updated by the functions `define-coding-system' and
  `define-coding-system-alias'.")

(defvar standard-translation-table-for-encode nil
  "Table for translating characters while encoding.")

(defvar coding-system-for-write nil
  "Specify the coding system for write operations.
  Programs bind this variable with `let', but you should not set it globally.
  If the value is a coding system, it is used for encoding of output,
  when writing it to a file and when sending it to a file or subprocess.

  If this does not specify a coding system, an appropriate element
  is used from one of the coding system alists.
  There are three such tables: `file-coding-system-alist',
  `process-coding-system-alist', and `network-coding-system-alist'.
  For output to files, if the above procedure does not specify a coding system,
  the value of `buffer-file-coding-system' is used.")

(defvar enable-character-translation nil
  "*Non-nil enables character translation while encoding and decoding.")

(defvar eol-mnemonic-unix ":"
  "*String displayed in mode line for UNIX-like (LF) end-of-line format.

  You can customize this variable.")

(defvar coding-system-for-read nil
  "Specify the coding system for read operations.
  It is useful to bind this variable with `let', but do not set it globally.
  If the value is a coding system, it is used for decoding on read operation.
  If not, an appropriate element is used from one of the coding system alists.
  There are three such tables: `file-coding-system-alist',
  `process-coding-system-alist', and `network-coding-system-alist'.")

(defvar eol-mnemonic-mac "/"
  "*String displayed in mode line for MAC-like (CR) end-of-line format.

  You can customize this variable.")

(defvar inherit-process-coding-system nil
  "Non-nil means process buffer inherits coding system of process output.
  Bind it to t if the process output is to be treated as if it were a file
  read from some filesystem.")

(defvar select-safe-coding-system-function nil
  "Function to call to select safe coding system for encoding a text.

  If set, this function is called to force a user to select a proper
  coding system which can encode the text in the case that a default
  coding system used in each operation can't encode the text.  The
  function should take care that the buffer is not modified while
  the coding system is being selected.

  The default value is `select-safe-coding-system' (which see).")

(defvar eol-mnemonic-dos "\\"
  "*String displayed in mode line for DOS-like (CRLF) end-of-line format.

  You can customize this variable.")

(defvar last-code-conversion-error nil
  "Error status of the last code conversion.

  When an error was detected in the last code conversion, this variable
  is set to one of the following symbols.
    `insufficient-source'
    `inconsistent-eol'
    `invalid-source'
    `interrupted'
    `insufficient-memory'
  When no error was detected, the value doesn't change.  So, to check
  the error status of a code conversion by this variable, you must
  explicitly set this variable to nil before performing code
  conversion.")

(defvar file-coding-system-alist nil
  "Alist to decide a coding system to use for a file I/O operation.
  The format is ((PATTERN . VAL) ...),
  where PATTERN is a regular expression matching a file name,
  VAL is a coding system, a cons of coding systems, or a function symbol.
  If VAL is a coding system, it is used for both decoding and encoding
  the file contents.
  If VAL is a cons of coding systems, the car part is used for decoding,
  and the cdr part is used for encoding.
  If VAL is a function symbol, the function must return a coding system
  or a cons of coding systems which are used as above.  The function is
  called with an argument that is a list of the arguments with which
  `find-operation-coding-system' was called.  If the function can't decide
  a coding system, it can return `undecided' so that the normal
  code-detection is performed.

  See also the function `find-operation-coding-system'
  and the variable `auto-coding-alist'.

  You can customize this variable.")

(defvar network-coding-system-alist nil
  "Alist to decide a coding system to use for a network I/O operation.
  The format is ((PATTERN . VAL) ...),
  where PATTERN is a regular expression matching a network service name
  or is a port number to connect to,
  VAL is a coding system, a cons of coding systems, or a function symbol.
  If VAL is a coding system, it is used for both decoding what received
  from the network stream and encoding what sent to the network stream.
  If VAL is a cons of coding systems, the car part is used for decoding,
  and the cdr part is used for encoding.
  If VAL is a function symbol, the function must return a coding system
  or a cons of coding systems which are used as above.

  See also the function `find-operation-coding-system'.")

(defvar default-process-coding-system nil
  "Cons of coding systems used for process I/O by default.
  The car part is used for decoding a process output,
  the cdr part is used for encoding a text to be sent to a process.")

(defvar inhibit-eol-conversion nil
  "*Non-nil means always inhibit code conversion of end-of-line format.
  See info node `Coding Systems' and info node `Text and Binary' concerning
  such conversion.

  You can customize this variable.")

(defvar coding-system-alist nil
  "Alist of coding system names.
  Each element is one element list of coding system name.
  This variable is given to `completing-read' as COLLECTION argument.

  Do not alter the value of this variable manually.  This variable should be
  updated by the functions `make-coding-system' and
  `define-coding-system-alias'.")

(defvar coding-system-require-warning nil
  "Internal use only.
  If non-nil, on writing a file, `select-safe-coding-system-function' is
  called even if `coding-system-for-write' is non-nil.  The command
  `universal-coding-system-argument' binds this variable to t temporarily.")

(defvar locale-coding-system nil
  "Coding system to use with system messages.
  Also used for decoding keyboard input on X Window system.")

(defvar translation-table-for-input nil
  "Char table for translating self-inserting characters.
  This is applied to the result of input methods, not their input.
  See also `keyboard-translate-table'.

  Use of this variable for character code unification was rendered
  obsolete in Emacs 23.1 and later, since Unicode is now the basis of
  internal character representation.")

(defvar coding-category-list nil
  "List of coding-categories (symbols) ordered by priority.

  On detecting a coding system, Emacs tries code detection algorithms
  associated with each coding-category one by one in this order.  When
  one algorithm agrees with a byte sequence of source text, the coding
  system bound to the corresponding coding-category is selected.

  Don't modify this variable directly, but use `set-coding-system-priority'.")

(defvar process-coding-system-alist nil
  "Alist to decide a coding system to use for a process I/O operation.
  The format is ((PATTERN . VAL) ...),
  where PATTERN is a regular expression matching a program name,
  VAL is a coding system, a cons of coding systems, or a function symbol.
  If VAL is a coding system, it is used for both decoding what received
  from the program and encoding what sent to the program.
  If VAL is a cons of coding systems, the car part is used for decoding,
  and the cdr part is used for encoding.
  If VAL is a function symbol, the function must return a coding system
  or a cons of coding systems which are used as above.

  See also the function `find-operation-coding-system'.")

(declare check-coding-system)

(fns/put 'translation-table 'char-table-extra-slots 2)

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
  ({"\n" '0 "\r\n" 1 "\r" 2} (System/lineSeparator)))

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
  (interactive "r\nzCoding system: "))

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
  (interactive "r\nzCoding system: "))

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
  (check-coding-system coding-system)
  (el/setq last-coding-system-used coding-system)
  string)

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
  'iso-latin-1-unix)

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
  (check-coding-system coding-system)
  (el/setq last-coding-system-used coding-system)
  string)

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
  (if (nil? coding-system)
    coding-system
    (el/throw 'coding-system-error coding-system)))

(defun coding-system-p (object)
  "Return t if OBJECT is nil or a coding-system.
  See the documentation of `define-coding-system' for information
  about coding-system objects."
  )

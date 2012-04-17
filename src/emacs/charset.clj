(ns
 emacs.charset
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun sort-charsets (lisp-object charsets)
  "Sort charset list CHARSETS by a priority of each charset.
  Return the sorted list.  CHARSETS is modified by side effects.
  See also `charset-priority-list' and `set-charset-priority'."
  )

(defun charset-plist (charset)
  "Return the property list of CHARSET."
  )

(defun set-charset-plist (charset plist)
  "Set CHARSET's property list to PLIST."
  )

(defun charset-after (&optional pos)
  "Return charset of a character in the current buffer at position POS.
  If POS is nil, it defauls to the current point.
  If POS is out of range, the value is nil."
  )

(defun define-charset-internal (&rest args)
  "For internal use only."
  )

(defun define-charset-alias (alias charset)
  "Define ALIAS as an alias for charset CHARSET."
  )

(defun charsetp (object)
  "Return non-nil if and only if OBJECT is a charset."
  )

(defun encode-char (ch charset &optional restriction)
  "Encode the character CH into a code-point of CHARSET.
  Return nil if CHARSET doesn't include CH."
  )

(defun charset-id-internal (&optional charset)
  "Internal use only.
  Return charset identification number of CHARSET."
  )

(defun split-char (ch)
  "Return list of charset and one to four position-codes of CH.
  The charset is decided by the current priority order of charsets.
  A position-code is a byte value of each dimension of the code-point of
  CH in the charset."
  )

(defun find-charset-region (beg end &optional table)
  "Return a list of charsets in the region between BEG and END.
  BEG and END are buffer positions.
  Optional arg TABLE if non-nil is a translation table to look up."
  )

(defun unify-charset (charset &optional unify-map deunify)
  "Unify characters of CHARSET with Unicode.
  This means reading the relevant file and installing the table defined
  by CHARSET's `:unify-map' property."
  )

(defun find-charset-string (str &optional table)
  "Return a list of charsets in STR.
  Optional arg TABLE if non-nil is a translation table to look up."
  )

(defun set-charset-priority (&rest charsets)
  "Assign higher priority to the charsets given as arguments."
  )

(defun declare-equiv-charset (dimension chars final-char charset)
  "Declare an equivalent charset for ISO-2022 decoding."
  )

(defun clear-charset-maps ()
  "Internal use only.
  Clear temporary charset mapping tables.
  It should be called only from temacs invoked for dumping."
  )

(defun get-unused-iso-final-char (dimension chars)
  "Return an unused ISO final char for a charset of DIMENSION and CHARS.
  DIMENSION is the number of bytes to represent a character: 1 or 2.
  CHARS is the number of characters in a dimension: 94 or 96."
  )

(defun charset-priority-list (&optional highestp)
  "Return the list of charsets ordered by priority.
  HIGHESTP non-nil means just return the highest priority one."
  )

(defun map-charset-chars (function charset &optional arg from-code to-code)
  "Call FUNCTION for all characters in CHARSET.
  FUNCTION is called with an argument RANGE and the optional 3rd
  argument ARG."
  )

(defun make-char (charset &optional code1 code2 code3 code4)
  "Return a character of CHARSET whose position codes are CODEn."
  )

(defun char-charset (ch &optional restriction)
  "Return the charset of highest priority that contains CH.
  If optional 2nd arg RESTRICTION is non-nil, it is a list of charsets
  from which to find the charset.  It may also be a coding system.  In
  that case, find the charset from what supported by that coding system."
  )

(defun decode-char (charset code-point &optional restriction)
  "Decode the pair of CHARSET and CODE-POINT into a character.
  Return nil if CODE-POINT is not valid in CHARSET."
  )

(defun iso-charset (dimension chars final-char)
  "Return charset of ISO's specification DIMENSION, CHARS, and FINAL-CHAR."
  )

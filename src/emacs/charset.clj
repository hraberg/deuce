(ns emacs.charset (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun sort-charsets (lisp-object charsets)
  "Sort charset list CHARSETS by a priority of each charset.\nReturn the sorted list.  CHARSETS is modified by side effects.\n"
  )

(defun charset-plist (charset)
  )

(defun set-charset-plist (charset plist)
  )

(defun charset-after (&optional pos)
  "Return charset of a character in the current buffer at position POS.\nIf POS is nil, it defauls to the current point.\n"
  )

(defun define-charset-internal (dot-dot-dot)
  )

(defun define-charset-alias (alias charset)
  )

(defun charsetp (object)
  )

(defun encode-char (ch charset &optional restriction)
  "Encode the character CH into a code-point of CHARSET.\nReturn nil if CHARSET doesn't include CH."
  )

(defun charset-id-internal (&optional charset)
  "Internal use only.\n"
  )

(defun split-char (ch)
  "Return list of charset and one to four position-codes of CH.\nThe charset is decided by the current priority order of charsets.\nA position-code is a byte value of each dimension of the code-point of\n"
  )

(defun find-charset-region (beg end &optional table)
  "Return a list of charsets in the region between BEG and END.\nBEG and END are buffer positions.\nOptional arg TABLE if non-nil is a translation table to look up."
  )

(defun unify-charset (charset &optional unify-map deunify)
  "Unify characters of CHARSET with Unicode.\nThis means reading the relevant file and installing the table defined\nby CHARSET's `:unify-map' property."
  )

(defun find-charset-string (str &optional table)
  "Return a list of charsets in STR.\nOptional arg TABLE if non-nil is a translation table to look up."
  )

(defun set-charset-priority (&rest charsets)
  )

(defun declare-equiv-charset (dimension chars final-char charset)
  "Declare an equivalent charset for ISO-2022 decoding."
  )

(defun clear-charset-maps ()
  "Internal use only.\nClear temporary charset mapping tables.\n"
  )

(defun get-unused-iso-final-char (dimension chars)
  "Return an unused ISO final char for a charset of DIMENSION and CHARS.\nDIMENSION is the number of bytes to represent a character: 1 or 2.\nCHARS is the number of characters in a dimension: 94 or 96."
  )

(defun charset-priority-list (&optional highestp)
  "Return the list of charsets ordered by priority.\n"
  )

(defun map-charset-chars (function charset &optional arg from-code to-code)
  "Call FUNCTION for all characters in CHARSET.\nFUNCTION is called with an argument RANGE and the optional 3rd\nargument ARG."
  )

(defun make-char (charset &optional code1 code2 code3 code4)
  "Return a character of CHARSET whose position codes are CODEn."
  )

(defun char-charset (ch &optional restriction)
  "Return the charset of highest priority that contains CH.\nIf optional 2nd arg RESTRICTION is non-nil, it is a list of charsets\nfrom which to find the charset.  It may also be a coding system.  In\nthat case, find the charset from what supported by that coding system.copy-file is an interactive built-in function in `C source code'."
  )

(defun decode-char (charset code-point &optional restriction)
  "Decode the pair of CHARSET and CODE-POINT into a character.\nReturn nil if CODE-POINT is not valid in CHARSET."
  )

(defun iso-charset (dimension chars final-char)
  "Return charset of ISO's specification DIMENSION, CHARS, and FINAL-CHAR."
  )

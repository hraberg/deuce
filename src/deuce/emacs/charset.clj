(ns deuce.emacs.charset
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c])
  (:refer-clojure :exclude []))

(defvar inhibit-load-charset-map nil
  "Inhibit loading of charset maps.  Used when dumping Emacs.")

(defvar current-iso639-language nil
  "ISO639 language mnemonic symbol for the current language environment.
  If the current language environment is for multiple languages (e.g. \"Latin-1\"),
  the value may be a list of mnemonics.")

(defvar charset-list nil
  "List of all charsets ever defined.")

(defvar charset-map-path nil
  "*List of directories to search for charset map files.

  You can customize this variable.")

(defun sort-charsets (charsets)
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
  If POS is nil, it defaults to the current point.
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
  Return nil if CHARSET doesn't include CH.

  Optional argument RESTRICTION specifies a way to map CH to a
  code-point in CCS.  Currently not supported and just ignored."
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
  Optional arg TABLE if non-nil is a translation table to look up.

  If the current buffer is unibyte, the returned list may contain
  only `ascii', `eight-bit-control', and `eight-bit-graphic'."
  )

(defun unify-charset (charset &optional unify-map deunify)
  "Unify characters of CHARSET with Unicode.
  This means reading the relevant file and installing the table defined
  by CHARSET's `:unify-map' property.

  Optional second arg UNIFY-MAP is a file name string or a vector.  It has
  the same meaning as the `:unify-map' attribute in the function
  `define-charset' (which see).

  Optional third argument DEUNIFY, if non-nil, means to de-unify CHARSET."
  )

(defun find-charset-string (str &optional table)
  "Return a list of charsets in STR.
  Optional arg TABLE if non-nil is a translation table to look up.

  If STR is unibyte, the returned list may contain
  only `ascii', `eight-bit-control', and `eight-bit-graphic'."
  )

(defun set-charset-priority (&rest charsets)
  "Assign higher priority to the charsets given as arguments."
  )

(defun declare-equiv-charset (dimension chars final-char charset)
  "Declare an equivalent charset for ISO-2022 decoding.

  On decoding by an ISO-2022 base coding system, when a charset
  specified by DIMENSION, CHARS, and FINAL-CHAR is designated, behave as
  if CHARSET is designated instead."
  )

(defun clear-charset-maps ()
  "Internal use only.
  Clear temporary charset mapping tables.
  It should be called only from temacs invoked for dumping."
  )

(defun get-unused-iso-final-char (dimension chars)
  "Return an unused ISO final char for a charset of DIMENSION and CHARS.
  DIMENSION is the number of bytes to represent a character: 1 or 2.
  CHARS is the number of characters in a dimension: 94 or 96.

  This final char is for private use, thus the range is `0' (48) .. `?' (63).
  If there's no unused final char for the specified kind of charset,
  return nil."
  )

(defun charset-priority-list (&optional highestp)
  "Return the list of charsets ordered by priority.
  HIGHESTP non-nil means just return the highest priority one."
  )

(defun map-charset-chars (function charset &optional arg from-code to-code)
  "Call FUNCTION for all characters in CHARSET.
  FUNCTION is called with an argument RANGE and the optional 3rd
  argument ARG.

  RANGE is a cons (FROM .  TO), where FROM and TO indicate a range of
  characters contained in CHARSET.

  The optional 4th and 5th arguments FROM-CODE and TO-CODE specify the
  range of code points (in CHARSET) of target characters."
  )

(defun make-char (charset &optional code1 code2 code3 code4)
  "Return a character of CHARSET whose position codes are CODEn.

  CODE1 through CODE4 are optional, but if you don't supply sufficient
  position codes, it is assumed that the minimum code in each dimension
  is specified."
  )

(defun char-charset (ch &optional restriction)
  "Return the charset of highest priority that contains CH.
  If optional 2nd arg RESTRICTION is non-nil, it is a list of charsets
  from which to find the charset.  It may also be a coding system.  In
  that case, find the charset from what supported by that coding system."
  )

(defun decode-char (charset code-point &optional restriction)
  "Decode the pair of CHARSET and CODE-POINT into a character.
  Return nil if CODE-POINT is not valid in CHARSET.

  CODE-POINT may be a cons (HIGHER-16-BIT-VALUE . LOWER-16-BIT-VALUE).

  Optional argument RESTRICTION specifies a way to map the pair of CCS
  and CODE-POINT to a character.  Currently not supported and just ignored."
  )

(defun iso-charset (dimension chars final-char)
  "Return charset of ISO's specification DIMENSION, CHARS, and FINAL-CHAR.

  ISO 2022's designation sequence (escape sequence) distinguishes charsets
  by their DIMENSION, CHARS, and FINAL-CHAR,
  whereas Emacs distinguishes them by charset symbol.
  See the documentation of the function `charset-info' for the meanings of
  DIMENSION, CHARS, and FINAL-CHAR."
  )

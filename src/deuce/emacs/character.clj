(ns
 deuce.emacs.character
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun unibyte-char-to-multibyte (ch)
  "Convert the byte CH to multibyte character."
  )

(defun multibyte-char-to-unibyte (ch)
  "Convert the multibyte character CH to a byte.
  If the multibyte character does not represent a byte, return -1."
  )

(defun char-resolve-modifiers (char)
  "Resolve modifiers in the character CHAR.
  The value is a character with modifiers resolved into the character
  code.  Unresolved modifiers are kept in the value."
  )

(defun string-width (string)
  "Return width of STRING when displayed in the current buffer.
  Width is measured by how many columns it occupies on the screen.
  When calculating width of a multibyte character in STRING,
  only the base leading-code is considered; the validity of
  the following bytes is not checked.  Tabs in STRING are always
  taken to occupy `tab-width' columns."
  )

(defun char-width (char)
  "Return width of CHAR when displayed in the current buffer.
  The width is measured by how many columns it occupies on the screen.
  Tab is taken to occupy `tab-width' columns."
  )

(defun characterp (object)
  "Return non-nil if OBJECT is a character."
  )

(defun unibyte-string (&rest bytes)
  "Concatenate all the argument bytes and make the result a unibyte string."
  )

(defun get-byte (&optional position string)
  "Return a byte value of a character at point.
  Optional 1st arg POSITION, if non-nil, is a position of a character to get
  a byte value.
  Optional 2nd arg STRING, if non-nil, is a string of which first
  character is a target to get a byte value.  In this case, POSITION, if
  non-nil, is an index of a target character in the string.
  
  If the current buffer (or STRING) is multibyte, and the target
  character is not ASCII nor 8-bit character, an error is signaled."
  )

(defun max-char ()
  "Return the character of the maximum code."
  )

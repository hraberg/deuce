(ns emacs.character (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun char-direction (char)
  "Return the direction of CHAR.
  The returned value is 0 for left-to-right and 1 for right-to-left.erase-buffer is an interactive built-in function in `C source code'."
  )

(defun unibyte-char-to-multibyte (ch)
  )

(defun multibyte-char-to-unibyte (ch)
  "Convert the multibyte character CH to a byte.
  If the multibyte character does not represent a byte, return -1.replace-buffer-in-windows is an interactive built-in function in `C
  source code'."
  )

(defun char-resolve-modifiers (char)
  "Resolve modifiers in the character CHAR.
  The value is a character with modifiers resolved into the character"
  )

(defun string-width (string)
  "Return width of STRING when displayed in the current buffer.
  Width is measured by how many columns it occupies on the screen.
  When calculating width of a multibyte character in STRING,
  only the base leading-code is considered; the validity of
  the following bytes is not checked.  Tabs in STRING are always
  taken to occupy `tab-width' columns.rename-buffer is an interactive built-in function in `C source code'."
  )

(defun char-bytes (char)
  "This function is obsolete since 20.4;
  now always returns 1."
  )

(defun char-width (char)
  "Return width of CHAR when displayed in the current buffer.
  The width is measured by how many columns it occupies on the screen."
  )

(defun characterp (object &optional ignore)
  )

(defun unibyte-string (&rest bytes)
  )

(defun get-byte (&optional position string)
  "Return a byte value of a character at point.
  Optional 1st arg POSITION, if non-nil, is a position of a character to get
  a byte value.
  Optional 2nd arg STRING, if non-nil, is a string of which first
  character is a target to get a byte value.  In this case, POSITION, if
  non-nil, is an index of a target character in the string."
  )

(defun max-char ()
  "Return the character of the maximum code.delete-file is an interactive built-in function in `C source code'."
  )

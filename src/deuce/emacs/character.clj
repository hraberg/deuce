(ns deuce.emacs.character
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c])
  (:refer-clojure :exclude []))

(defvar auto-fill-chars nil
  "A char-table for characters which invoke auto-filling.
  Such characters have value t in this table.")

(defvar printable-chars nil
  "A char-table for each printable character.")

(defvar unicode-category-table nil
  "Char table of Unicode's \"General Category\".
  All Unicode characters have one of the following values (symbol):
    Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Pc, Pd, Ps, Pe, Pi, Pf, Po,
    Sm, Sc, Sk, So, Zs, Zl, Zp, Cc, Cf, Cs, Co, Cn
  See The Unicode Standard for the meaning of those values.")

(defvar char-width-table nil
  "A char-table for width (columns) of each character.")

(defvar char-script-table nil
  "Char table of script symbols.
  It has one extra slot whose value is a list of script symbols.")

(defvar translation-table-vector nil
  "Vector recording all translation tables ever defined.
  Each element is a pair (SYMBOL . TABLE) relating the table to the
  symbol naming it.  The ID of a translation table is an index into this vector.")

(defvar script-representative-chars nil
  "Alist of scripts vs the representative characters.
  Each element is a cons (SCRIPT . CHARS).
  SCRIPT is a symbol representing a script or a subgroup of a script.
  CHARS is a list or a vector of characters.
  If it is a list, all characters in the list are necessary for supporting SCRIPT.
  If it is a vector, one of the characters in the vector is necessary.
  This variable is used to find a font for a specific script.")

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

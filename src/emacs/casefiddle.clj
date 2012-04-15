(ns emacs.casefiddle (use [deuce.core]) (:refer-clojure :only []))

(defun upcase (obj)
  "Convert argument to upper case and return that.\nThe argument may be a character or string.  The result has the same type.\nThe argument object is not altered--the value is a copy.\n"
  )

(defun upcase-initials (obj)
  "Convert the initial of each word in the argument to upper case.\nDo not change the other letters of each word.\nThe argument may be a character or string.  The result has the same type.\n"
  )

(defun capitalize (obj)
  "Convert argument to capitalized form and return that.\nThis means that each word's first character is upper case\nand the rest is lower case.\nThe argument may be a character or string.  The result has the same type.\n"
  )

(defun downcase (obj)
  "Convert argument to lower case and return that.\nThe argument may be a character or string.  The result has the same type.\n"
  )

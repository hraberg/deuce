(ns emacs.casefiddle (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun upcase (obj)
  "Convert argument to upper case and return that.
  The argument may be a character or string.  The result has the same type.
  The argument object is not altered--the value is a copy.
  See also `capitalize', `downcase' and `upcase-initials'."
  )

(defun upcase-initials (obj)
  "Convert the initial of each word in the argument to upper case.
  Do not change the other letters of each word.
  The argument may be a character or string.  The result has the same type.
  The argument object is not altered--the value is a copy."
  )

(defun capitalize (obj)
  "Convert argument to capitalized form and return that.
  This means that each word's first character is upper case
  and the rest is lower case.
  The argument may be a character or string.  The result has the same type.
  The argument object is not altered--the value is a copy."
  )

(defun downcase (obj)
  "Convert argument to lower case and return that.
  The argument may be a character or string.  The result has the same type.
  The argument object is not altered--the value is a copy."
  )

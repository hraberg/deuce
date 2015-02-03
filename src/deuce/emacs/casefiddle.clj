(ns deuce.emacs.casefiddle
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c]
            [clojure.string :as s])
  (:refer-clojure :exclude []))

(defun upcase-word (arg)
  "Convert following word (or ARG words) to upper case, moving over.
  With negative argument, convert previous words but do not move.
  See also `capitalize-word'."
  (interactive "p"))

(defun upcase (obj)
  "Convert argument to upper case and return that.
  The argument may be a character or string.  The result has the same type.
  The argument object is not altered--the value is a copy.
  See also `capitalize', `downcase' and `upcase-initials'."
  (if ((some-fn char? integer?) obj)
    (int (Character/toUpperCase (int obj)))
    (s/upper-case obj)))

(defun capitalize-word (arg)
  "Capitalize the following word (or ARG words), moving over.
  This gives the word(s) a first character in upper case
  and the rest lower case.
  With negative argument, capitalize previous words but do not move."
  (interactive "p"))

(defun downcase-region (beg end)
  "Convert the region to lower case.  In programs, wants two arguments.
  These arguments specify the starting and ending character numbers of
  the region to operate on.  When used as a command, the text between
  point and the mark is operated on."
  (interactive "r"))

(defun capitalize-region (beg end)
  "Convert the region to capitalized form.
  Capitalized form means each word's first character is upper case
  and the rest of it is lower case.
  In programs, give two arguments, the starting and ending
  character positions to operate on."
  (interactive "r"))

(defun upcase-initials (obj)
  "Convert the initial of each word in the argument to upper case.
  Do not change the other letters of each word.
  The argument may be a character or string.  The result has the same type.
  The argument object is not altered--the value is a copy."
  (s/replace obj #"\w+" #(apply str (s/upper-case (first %)) (rest %))))

(defun downcase-word (arg)
  "Convert following word (or ARG words) to lower case, moving over.
  With negative argument, convert previous words but do not move."
  (interactive "p"))

(defun upcase-region (beg end)
  "Convert the region to upper case.  In programs, wants two arguments.
  These arguments specify the starting and ending character numbers of
  the region to operate on.  When used as a command, the text between
  point and the mark is operated on.
  See also `capitalize-region'."
  (interactive "r"))

(defun capitalize (obj)
  "Convert argument to capitalized form and return that.
  This means that each word's first character is upper case
  and the rest is lower case.
  The argument may be a character or string.  The result has the same type.
  The argument object is not altered--the value is a copy."
  (s/capitalize obj))

(defun upcase-initials-region (beg end)
  "Upcase the initial of each word in the region.
  Subsequent letters of each word are not changed.
  In programs, give two arguments, the starting and ending
  character positions to operate on."
  (interactive "r"))

(defun downcase (obj)
  "Convert argument to lower case and return that.
  The argument may be a character or string.  The result has the same type.
  The argument object is not altered--the value is a copy."
  (if ((some-fn char? integer?) obj)
    (int (Character/toLowerCase (int obj)))
    (s/lower-case obj)))

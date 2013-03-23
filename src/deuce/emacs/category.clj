(ns deuce.emacs.category
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c])
  (:refer-clojure :exclude []))

(defvar word-separating-categories nil
  "List of pair (cons) of categories to determine word boundary.
  See the documentation of the variable `word-combining-categories'.")

(defvar word-combining-categories nil
  "List of pair (cons) of categories to determine word boundary.

  Emacs treats a sequence of word constituent characters as a single
  word (i.e. finds no word boundary between them) only if they belong to
  the same script.  But, exceptions are allowed in the following cases.

  (1) The case that characters are in different scripts is controlled
  by the variable `word-combining-categories'.

  Emacs finds no word boundary between characters of different scripts
  if they have categories matching some element of this list.

  More precisely, if an element of this list is a cons of category CAT1
  and CAT2, and a multibyte character C1 which has CAT1 is followed by
  C2 which has CAT2, there's no word boundary between C1 and C2.

  For instance, to tell that Han characters followed by Hiragana
  characters can form a single word, the element `(?C . ?H)' should be
  in this list.

  (2) The case that character are in the same script is controlled by
  the variable `word-separating-categories'.

  Emacs finds a word boundary between characters of the same script
  if they have categories matching some element of this list.

  More precisely, if an element of this list is a cons of category CAT1
  and CAT2, and a multibyte character C1 which has CAT1 but not CAT2 is
  followed by C2 which has CAT2 but not CAT1, there's a word boundary
  between C1 and C2.

  For instance, to tell that there's a word boundary between Hiragana
  and Katakana (both are in the same script `kana'),
  the element `(?H . ?K) should be in this list.")

(fns/put 'category-table 'char-table-extra-slots 2)

(defun standard-category-table ()
  "Return the standard category table.
  This is the one used for new buffers."
  )

(defun make-category-table ()
  "Construct a new and empty category table and return it."
  )

(defun copy-category-table (&optional table)
  "Construct a new category table and return it.
  It is a copy of the TABLE, which defaults to the standard category table."
  )

(defun char-category-set (char)
  "Return the category set of CHAR."
  )

(defun make-category-set (categories)
  "Return a newly created category-set which contains CATEGORIES.
  CATEGORIES is a string of category mnemonics.
  The value is a bool-vector which has t at the indices corresponding to
  those categories."
  )

(defun category-set-mnemonics (category-set)
  "Return a string containing mnemonics of the categories in CATEGORY-SET.
  CATEGORY-SET is a bool-vector, and the categories \"in\" it are those
  that are indexes where t occurs in the bool-vector.
  The return value is a string containing those same categories."
  )

(defun set-category-table (table)
  "Specify TABLE as the category table for the current buffer.
  Return TABLE."
  )

(defun category-table-p (arg)
  "Return t if ARG is a category table."
  )

(defun category-table ()
  "Return the current category table.
  This is the one specified by the current buffer."
  )

(defun modify-category-entry (character category &optional table reset)
  "Modify the category set of CHARACTER by adding CATEGORY to it.
  The category is changed only for table TABLE, which defaults to
  the current buffer's category table.
  CHARACTER can be either a single character or a cons representing the
  lower and upper ends of an inclusive character range to modify.
  If optional fourth argument RESET is non-nil,
  then delete CATEGORY from the category set instead of adding it."
  )

(defun category-docstring (category &optional table)
  "Return the documentation string of CATEGORY, as defined in TABLE.
  TABLE should be a category table and defaults to the current buffer's
  category table."
  )

(defun define-category (category docstring &optional table)
  "Define CATEGORY as a category which is described by DOCSTRING.
  CATEGORY should be an ASCII printing character in the range ` ' to `~'.
  DOCSTRING is the documentation string of the category.  The first line
  should be a terse text (preferably less than 16 characters),
  and the rest lines should be the full description.
  The category is defined only in category table TABLE, which defaults to
  the current buffer's category table."
  )

(defun get-unused-category (&optional table)
  "Return a category which is not yet defined in TABLE.
  If no category remains available, return nil.
  The optional argument TABLE specifies which category table to modify;
  it defaults to the current buffer's category table."
  )

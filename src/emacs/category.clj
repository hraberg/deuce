(ns
 emacs.category
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

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

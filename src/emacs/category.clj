(ns emacs.category (use [deuce.core]) (:refer-clojure :only []))

(defun standard-category-table ()
  "Return the standard category table.\n"
  )

(defun make-category-table ()
  )

(defun copy-category-table (&optional table)
  "Construct a new category table and return it.\n"
  )

(defun char-category-set (char)
  )

(defun make-category-set (categories)
  "Return a newly created category-set which contains CATEGORIES.\nCATEGORIES is a string of category mnemonics.\nThe value is a bool-vector which has t at the indices corresponding to\n"
  )

(defun category-set-mnemonics (category-set)
  "Return a string containing mnemonics of the categories in CATEGORY-SET.\nCATEGORY-SET is a bool-vector, and the categories \"in\" it are those\nthat are indexes where t occurs in the bool-vector.\n"
  )

(defun set-category-table (table)
  "Specify TABLE as the category table for the current buffer.\n"
  )

(defun category-table-p (arg)
  )

(defun category-table ()
  "Return the current category table.\n"
  )

(defun modify-category-entry (character category &optional table reset)
  "Modify the category set of CHARACTER by adding CATEGORY to it.\nThe category is changed only for table TABLE, which defaults to\nthe current buffer's category table.\nCHARACTER can be either a single character or a cons representing the\nlower and upper ends of an inclusive character range to modify.\nIf optional fourth argument RESET is non-nil,\n"
  )

(defun category-docstring (category &optional table)
  "Return the documentation string of CATEGORY, as defined in TABLE.\nTABLE should be a category table and defaults to the current buffer's\n"
  )

(defun define-category (category docstring &optional table)
  "Define CATEGORY as a category which is described by DOCSTRING.\nCATEGORY should be an ASCII printing character in the range ` ' to `~'.\nDOCSTRING is the documentation string of the category.  The first line\nshould be a terse text (preferably less than 16 characters),\nand the rest lines should be the full description.\nThe category is defined only in category table TABLE, which defaults to\nthe current buffer's category table.write-region is an interactive built-in function in `C source code'."
  )

(defun get-unused-category (&optional table)
  "Return a category which is not yet defined in TABLE.\nIf no category remains available, return nil.\nThe optional argument TABLE specifies which category table to modify;\n"
  )

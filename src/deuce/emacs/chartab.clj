(ns
 deuce.emacs.chartab
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun char-table-parent (char-table)
  "Return the parent char-table of CHAR-TABLE.
  The value is either nil or another char-table.
  If CHAR-TABLE holds nil for a given character,
  then the actual applicable value is inherited from the parent char-table
  (or from its parents, if necessary)."
  )

(defun set-char-table-parent (char-table parent)
  "Set the parent char-table of CHAR-TABLE to PARENT.
  Return PARENT.  PARENT must be either nil or another char-table."
  )

(defun map-char-table (function char-table)
  "Call FUNCTION for each character in CHAR-TABLE that has non-nil value.
  FUNCTION is called with two arguments--a key and a value.
  The key is a character code or a cons of character codes specifying a
  range of characters that have the same value."
  )

(defun char-table-extra-slot (char-table n)
  "Return the value of CHAR-TABLE's extra-slot number N."
  )

(defun char-table-subtype (char-table)
  "Return the subtype of char-table CHAR-TABLE.  The value is a symbol."
  )

(defun set-char-table-range (char-table range value)
  "Set the value in CHAR-TABLE for a range of characters RANGE to VALUE.
  RANGE should be t (for all characters), nil (for the default value),
  a cons of character codes (for characters in the range),
  or a character code.  Return VALUE."
  )

(defun set-char-table-extra-slot (char-table n value)
  "Set CHAR-TABLE's extra-slot number N to VALUE."
  )

(defun make-char-table (purpose &optional init)
  "Return a newly created char-table, with purpose PURPOSE.
  Each element is initialized to INIT, which defaults to nil."
  )

(defun set-char-table-default (char-table ch value)
  "This function is obsolete since 23.1;
  generic characters no longer exist."
  )

(defun char-table-range (char-table range)
  "Return the value in CHAR-TABLE for a range of characters RANGE.
  RANGE should be nil (for the default value),
  a cons of character codes (for characters in the range), or a character code."
  )

(defun optimize-char-table (char-table &optional test)
  "Optimize CHAR-TABLE.
  TEST is the comparison function used to decide whether two entries are
  equivalent and can be merged.  It defaults to `equal'."
  )

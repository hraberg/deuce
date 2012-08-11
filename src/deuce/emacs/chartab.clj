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

(defun get-unicode-property-internal (char-table ch)
  "Return an element of CHAR-TABLE for character CH.
  CHAR-TABLE must be what returned by `unicode-property-table-internal'."
  )

(defun set-char-table-extra-slot (char-table n value)
  "Set CHAR-TABLE's extra-slot number N to VALUE."
  )

(defun unicode-property-table-internal (prop)
  "Return a char-table for Unicode character property PROP.
  Use `get-unicode-property-internal' and
  `put-unicode-property-internal' instead of `aref' and `aset' to get
  and put an element value."
  )

(defun make-char-table (purpose &optional init)
  "Return a newly created char-table, with purpose PURPOSE.
  Each element is initialized to INIT, which defaults to nil.
  
  PURPOSE should be a symbol.  If it has a `char-table-extra-slots'
  property, the property's value should be an integer between 0 and 10
  that specifies how many extra slots the char-table has.  Otherwise,
  the char-table has no extra slot."
  )

(defun set-char-table-default (char-table ch value)
  "This function is obsolete since 23.1;
  generic characters no longer exist.
  
  This function is obsolete and has no effect."
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

(defun put-unicode-property-internal (char-table ch value)
  "Set an element of CHAR-TABLE for character CH to VALUE.
  CHAR-TABLE must be what returned by `unicode-property-table-internal'."
  )

(ns emacs.chartab (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun char-table-parent (char-table)
  "Return the parent char-table of CHAR-TABLE.\nThe value is either nil or another char-table.\nIf CHAR-TABLE holds nil for a given character,\nthen the actual applicable value is inherited from the parent char-table\n"
  )

(defun set-char-table-parent (char-table parent)
  "Set the parent char-table of CHAR-TABLE to PARENT.\n"
  )

(defun map-char-table (function char-table)
  "Call FUNCTION for each character in CHAR-TABLE that has non-nil value.\nFUNCTION is called with two arguments--a key and a value.\nThe key is a character code or a cons of character codes specifying a\n"
  )

(defun char-table-extra-slot (char-table n)
  )

(defun char-table-subtype (char-table)
  )

(defun set-char-table-range (char-table range value)
  "Set the value in CHAR-TABLE for a range of characters RANGE to VALUE.\nRANGE should be t (for all characters), nil (for the default value),\na cons of character codes (for characters in the range),\n"
  )

(defun set-char-table-extra-slot (char-table n value)
  )

(defun make-char-table (purpose &optional init)
  "Return a newly created char-table, with purpose PURPOSE.\nEach element is initialized to INIT, which defaults to nil."
  )

(defun set-char-table-default (char-table ch value)
  "This function is obsolete since 23.1;\ngeneric characters no longer exist."
  )

(defun char-table-range (char-table range)
  "Return the value in CHAR-TABLE for a range of characters RANGE.\nRANGE should be nil (for the default value),\n"
  )

(defun optimize-char-table (char-table &optional test)
  "Optimize CHAR-TABLE.\nTEST is the comparison function used to decide whether two entries are\n"
  )

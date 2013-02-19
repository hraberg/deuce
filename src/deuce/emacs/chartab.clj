(ns deuce.emacs.chartab
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.data :as data]
            [deuce.emacs.fns :as fns])
  (:import [deuce.util Cons]
           [deuce.emacs.data CharTable])
  (:refer-clojure :exclude []))

(defvar char-code-property-alist nil
  "Alist of character property name vs char-table containing property values.
  Internal use only.")

(defun char-table-parent (char-table)
  "Return the parent char-table of CHAR-TABLE.
  The value is either nil or another char-table.
  If CHAR-TABLE holds nil for a given character,
  then the actual applicable value is inherited from the parent char-table
  (or from its parents, if necessary)."
  @(.parent char-table))

(defun set-char-table-parent (char-table parent)
  "Set the parent char-table of CHAR-TABLE to PARENT.
  Return PARENT.  PARENT must be either nil or another char-table."
  (reset! (.parent char-table) parent))

(defun map-char-table (function char-table)
  "Call FUNCTION for each character in CHAR-TABLE that has non-nil value.
  FUNCTION is called with two arguments--a key and a value.
  The key is a character code or a cons of character codes specifying a
  range of characters that have the same value."
  )

(defun char-table-extra-slot (char-table n)
  "Return the value of CHAR-TABLE's extra-slot number N."
  (aget (.extras char-table) n))

(defun char-table-subtype (char-table)
  "Return the subtype of char-table CHAR-TABLE.  The value is a symbol."
  (.purpose char-table))

(defun set-char-table-range (char-table range value)
  "Set the value in CHAR-TABLE for a range of characters RANGE to VALUE.
  RANGE should be t (for all characters), nil (for the default value),
  a cons of character codes (for characters in the range),
  or a character code.  Return VALUE."
  (doseq [n (if (instance? Cons range)
              (c/range (int (.car range)) (int (.cdr range)))
              (c/range (count (.contents char-table))))]
    (data/aset (.contents char-table) n (if (nil? range) (.defalt char-table) value)))
  value)

(defun get-unicode-property-internal (char-table ch)
  "Return an element of CHAR-TABLE for character CH.
  CHAR-TABLE must be what returned by `unicode-property-table-internal'."
  )

(defun set-char-table-extra-slot (char-table n value)
  "Set CHAR-TABLE's extra-slot number N to VALUE."
  (aset (.extras char-table) n value))

(defun unicode-property-table-internal (prop)
  "Return a char-table for Unicode character property PROP.
  Use `get-unicode-property-internal' and
  `put-unicode-property-internal' instead of `aref' and `aset' to get
  and put an element value."
  )

(def ^:private char-table-size 4194303)

(defun make-char-table (purpose &optional init)
  "Return a newly created char-table, with purpose PURPOSE.
  Each element is initialized to INIT, which defaults to nil.

  PURPOSE should be a symbol.  If it has a `char-table-extra-slots'
  property, the property's value should be an integer between 0 and 10
  that specifies how many extra slots the char-table has.  Otherwise,
  the char-table has no extra slot."
  (CharTable. init (atom nil) purpose
              (alloc/make-vector char-table-size init)
              (when-let [extras (fns/get purpose 'char-table-extra-slots)]
                (alloc/make-vector extras init))))

(defun set-char-table-default (char-table ch value)
  "This function is obsolete since 23.1;
  generic characters no longer exist.

  This function is obsolete and has no effect."
  nil)

(defun char-table-range (char-table range)
  "Return the value in CHAR-TABLE for a range of characters RANGE.
  RANGE should be nil (for the default value),
  a cons of character codes (for characters in the range), or a character code."
  )

(defun optimize-char-table (char-table &optional test)
  "Optimize CHAR-TABLE.
  TEST is the comparison function used to decide whether two entries are
  equivalent and can be merged.  It defaults to `equal'."
  nil)

(defun put-unicode-property-internal (char-table ch value)
  "Set an element of CHAR-TABLE for character CH to VALUE.
  CHAR-TABLE must be what returned by `unicode-property-table-internal'."
  )

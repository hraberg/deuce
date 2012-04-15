(ns emacs.textprop (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun next-char-property-change (position &optional limit)
  "Return the position of next text property or overlay change.\nThis scans characters forward in the current buffer from POSITION till\nit finds a change in some text property, or the beginning or end of an\noverlay, and returns the position of that.\nIf none is found up to (point-max), the function returns (point-max)."
  )

(defun next-property-change (position &optional object limit)
  "Return the position of next property change.\nScans characters forward from POSITION in OBJECT till it finds\na change in some text property, then returns the position of the change.\nIf the optional second argument OBJECT is a buffer (or nil, which means\nthe current buffer), POSITION is a buffer position (integer or marker).\nIf OBJECT is a string, POSITION is a 0-based index into it.\nReturn nil if the property is constant all the way to the end of OBJECT.\nIf the value is non-nil, it is a position greater than POSITION, never equal."
  )

(defun text-property-not-all (start end property value &optional object)
  "Check text from START to END for property PROPERTY not equalling VALUE.\nIf so, return the position of the first character whose property PROPERTY\nis not `eq' to VALUE.  Otherwise, return nil.\nIf the optional fifth argument OBJECT is a buffer (or nil, which means\nthe current buffer), START and END are buffer positions (integers or\n"
  )

(defun add-text-properties (start end properties &optional object)
  "Add properties to the text from START to END.\nThe third argument PROPERTIES is a property list\nspecifying the property values to add.  If the optional fourth argument\nOBJECT is a buffer (or nil, which means the current buffer),\nSTART and END are buffer positions (integers or markers).\nIf OBJECT is a string, START and END are 0-based indices into it.\n"
  )

(defun text-property-any (start end property value &optional object)
  "Check text from START to END for property PROPERTY equalling VALUE.\nIf so, return the position of the first character whose property PROPERTY\nis `eq' to VALUE.  Otherwise return nil.\nIf the optional fifth argument OBJECT is a buffer (or nil, which means\nthe current buffer), START and END are buffer positions (integers or\nmarkers).  If OBJECT is a string, START and END are 0-based indices into it.select-frame is an interactive built-in function in `C source code'."
  )

(defun put-text-property (start end property value &optional object)
  "Set one property of the text from START to END.\nThe third and fourth arguments PROPERTY and VALUE\nspecify the property to add.\nIf the optional fifth argument OBJECT is a buffer (or nil, which means\nthe current buffer), START and END are buffer positions (integers or\n"
  )

(defun remove-text-properties (start end properties &optional object)
  "Remove some properties from text from START to END.\nThe third argument PROPERTIES is a property list\nwhose property names specify the properties to remove.\n(The values stored in PROPERTIES are ignored.)\nIf the optional fourth argument OBJECT is a buffer (or nil, which means\nthe current buffer), START and END are buffer positions (integers or\nmarkers).  If OBJECT is a string, START and END are 0-based indices into it.\nReturn t if any property was actually removed, nil otherwise."
  )

(defun get-char-property (position prop &optional object)
  "Return the value of POSITION's property PROP, in OBJECT.\nBoth overlay properties and text properties are checked.\nOBJECT is optional and defaults to the current buffer.\nIf POSITION is at the end of OBJECT, the value is nil.\nIf OBJECT is a buffer, then overlay properties are considered as well as\ntext properties.\nIf OBJECT is a window, then that window's buffer is used, but window-specific\n"
  )

(defun next-single-property-change (position prop &optional object limit)
  "Return the position of next property change for a specific property.\nScans characters forward from POSITION till it finds\na change in the PROP property, then returns the position of the change.\nIf the optional third argument OBJECT is a buffer (or nil, which means\nthe current buffer), POSITION is a buffer position (integer or marker).\nIf OBJECT is a string, POSITION is a 0-based index into it.\nThe property values are compared with `eq'.\nReturn nil if the property is constant all the way to the end of OBJECT.\nIf the value is non-nil, it is a position greater than POSITION, never equal."
  )

(defun get-text-property (position prop &optional object)
  "Return the value of POSITION's property PROP, in OBJECT.\nOBJECT is optional and defaults to the current buffer.\n"
  )

(defun text-properties-at (position &optional object)
  "Return the list of properties of the character at POSITION in OBJECT.\nIf the optional second argument OBJECT is a buffer (or nil, which means\nthe current buffer), POSITION is a buffer position (integer or marker).\nIf OBJECT is a string, POSITION is a 0-based index into it.\n"
  )

(defun set-text-properties (start end properties &optional object)
  "Completely replace properties of text from START to END.\nThe third argument PROPERTIES is the new property list.\nIf the optional fourth argument OBJECT is a buffer (or nil, which means\nthe current buffer), START and END are buffer positions (integers or\nmarkers).  If OBJECT is a string, START and END are 0-based indices into it.\nIf PROPERTIES is nil, the effect is to remove all properties from\n"
  )

(defun previous-property-change (position &optional object limit)
  "Return the position of previous property change.\nScans characters backwards from POSITION in OBJECT till it finds\na change in some text property, then returns the position of the change.\nIf the optional second argument OBJECT is a buffer (or nil, which means\nthe current buffer), POSITION is a buffer position (integer or marker).\nIf OBJECT is a string, POSITION is a 0-based index into it.\nReturn nil if the property is constant all the way to the start of OBJECT.\nIf the value is non-nil, it is a position less than POSITION, never equal."
  )

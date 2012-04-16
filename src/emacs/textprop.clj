(ns emacs.textprop (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun next-char-property-change (position &optional limit)
  "Return the position of next text property or overlay change.
  This scans characters forward in the current buffer from POSITION till
  it finds a change in some text property, or the beginning or end of an
  overlay, and returns the position of that.
  If none is found up to (point-max), the function returns (point-max)."
  )

(defun next-property-change (position &optional object limit)
  "Return the position of next property change.
  Scans characters forward from POSITION in OBJECT till it finds
  a change in some text property, then returns the position of the change.
  If the optional second argument OBJECT is a buffer (or nil, which means
  the current buffer), POSITION is a buffer position (integer or marker).
  If OBJECT is a string, POSITION is a 0-based index into it.
  Return nil if the property is constant all the way to the end of OBJECT.
  If the value is non-nil, it is a position greater than POSITION, never equal."
  )

(defun text-property-not-all (start end property value &optional object)
  "Check text from START to END for property PROPERTY not equalling VALUE.
  If so, return the position of the first character whose property PROPERTY
  is not `eq' to VALUE.  Otherwise, return nil.
  If the optional fifth argument OBJECT is a buffer (or nil, which means
  the current buffer), START and END are buffer positions (integers or
  markers).  If OBJECT is a string, START and END are 0-based indices into it."
  )

(defun add-text-properties (start end properties &optional object)
  "Add properties to the text from START to END.
  The third argument PROPERTIES is a property list
  specifying the property values to add.  If the optional fourth argument
  OBJECT is a buffer (or nil, which means the current buffer),
  START and END are buffer positions (integers or markers).
  If OBJECT is a string, START and END are 0-based indices into it.
  Return t if any property value actually changed, nil otherwise."
  )

(defun text-property-any (start end property value &optional object)
  "Check text from START to END for property PROPERTY equalling VALUE.
  If so, return the position of the first character whose property PROPERTY
  is `eq' to VALUE.  Otherwise return nil.
  If the optional fifth argument OBJECT is a buffer (or nil, which means
  the current buffer), START and END are buffer positions (integers or
  markers).  If OBJECT is a string, START and END are 0-based indices into it.select-frame is an interactive built-in function in `C source code'."
  )

(defun put-text-property (start end property value &optional object)
  "Set one property of the text from START to END.
  The third and fourth arguments PROPERTY and VALUE
  specify the property to add.
  If the optional fifth argument OBJECT is a buffer (or nil, which means
  the current buffer), START and END are buffer positions (integers or
  markers).  If OBJECT is a string, START and END are 0-based indices into it."
  )

(defun remove-text-properties (start end properties &optional object)
  "Remove some properties from text from START to END.
  The third argument PROPERTIES is a property list
  whose property names specify the properties to remove.
  (The values stored in PROPERTIES are ignored.)
  If the optional fourth argument OBJECT is a buffer (or nil, which means
  the current buffer), START and END are buffer positions (integers or
  markers).  If OBJECT is a string, START and END are 0-based indices into it.
  Return t if any property was actually removed, nil otherwise."
  )

(defun get-char-property (position prop &optional object)
  "Return the value of POSITION's property PROP, in OBJECT.
  Both overlay properties and text properties are checked.
  OBJECT is optional and defaults to the current buffer.
  If POSITION is at the end of OBJECT, the value is nil.
  If OBJECT is a buffer, then overlay properties are considered as well as
  text properties.
  If OBJECT is a window, then that window's buffer is used, but window-specific
  overlays are considered only if they are associated with OBJECT."
  )

(defun next-single-property-change (position prop &optional object limit)
  "Return the position of next property change for a specific property.
  Scans characters forward from POSITION till it finds
  a change in the PROP property, then returns the position of the change.
  If the optional third argument OBJECT is a buffer (or nil, which means
  the current buffer), POSITION is a buffer position (integer or marker).
  If OBJECT is a string, POSITION is a 0-based index into it.
  The property values are compared with `eq'.
  Return nil if the property is constant all the way to the end of OBJECT.
  If the value is non-nil, it is a position greater than POSITION, never equal."
  )

(defun get-text-property (position prop &optional object)
  "Return the value of POSITION's property PROP, in OBJECT.
  OBJECT is optional and defaults to the current buffer.
  If POSITION is at the end of OBJECT, the value is nil."
  )

(defun text-properties-at (position &optional object)
  "Return the list of properties of the character at POSITION in OBJECT.
  If the optional second argument OBJECT is a buffer (or nil, which means
  the current buffer), POSITION is a buffer position (integer or marker).
  If OBJECT is a string, POSITION is a 0-based index into it.
  If POSITION is at the end of OBJECT, the value is nil."
  )

(defun set-text-properties (start end properties &optional object)
  "Completely replace properties of text from START to END.
  The third argument PROPERTIES is the new property list.
  If the optional fourth argument OBJECT is a buffer (or nil, which means
  the current buffer), START and END are buffer positions (integers or
  markers).  If OBJECT is a string, START and END are 0-based indices into it.
  If PROPERTIES is nil, the effect is to remove all properties from
  the designated part of OBJECT."
  )

(defun previous-property-change (position &optional object limit)
  "Return the position of previous property change.
  Scans characters backwards from POSITION in OBJECT till it finds
  a change in some text property, then returns the position of the change.
  If the optional second argument OBJECT is a buffer (or nil, which means
  the current buffer), POSITION is a buffer position (integer or marker).
  If OBJECT is a string, POSITION is a 0-based index into it.
  Return nil if the property is constant all the way to the start of OBJECT.
  If the value is non-nil, it is a position less than POSITION, never equal."
  )

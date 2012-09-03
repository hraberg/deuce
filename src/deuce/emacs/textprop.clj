(ns
 deuce.emacs.textprop
 (use [deuce.emacs-lisp :only (defun defvar)])
 (require [clojure.core :as c])
 (:refer-clojure :exclude []))

(defvar default-text-properties nil
  "Property-list used as default values.
  The value of a property in this list is seen as the value for every
  character that does not have its own value for that property.")

(defvar text-property-default-nonsticky nil
  "Alist of properties vs the corresponding non-stickiness.
  Each element has the form (PROPERTY . NONSTICKINESS).
  
  If a character in a buffer has PROPERTY, new text inserted adjacent to
  the character doesn't inherit PROPERTY if NONSTICKINESS is non-nil,
  inherits it if NONSTICKINESS is nil.  The `front-sticky' and
  `rear-nonsticky' properties of the character override NONSTICKINESS.")

(defvar char-property-alias-alist nil
  "Alist of alternative properties for properties without a value.
  Each element should look like (PROPERTY ALTERNATIVE1 ALTERNATIVE2...).
  If a piece of text has no direct value for a particular property, then
  this alist is consulted.  If that property appears in the alist, then
  the first non-nil value from the associated alternative properties is
  returned.")

(defvar inhibit-point-motion-hooks nil
  "If non-nil, don't run `point-left' and `point-entered' text properties.
  This also inhibits the use of the `intangible' text property.")

(defun next-char-property-change (position &optional limit)
  "Return the position of next text property or overlay change.
  This scans characters forward in the current buffer from POSITION till
  it finds a change in some text property, or the beginning or end of an
  overlay, and returns the position of that.
  If none is found up to (point-max), the function returns (point-max).
  
  If the optional second argument LIMIT is non-nil, don't search
  past position LIMIT; return LIMIT if nothing is found before LIMIT.
  LIMIT is a no-op if it is greater than (point-max)."
  )

(defun remove-list-of-text-properties (start end list-of-properties &optional object)
  "Remove some properties from text from START to END.
  The third argument LIST-OF-PROPERTIES is a list of property names to remove.
  If the optional fourth argument OBJECT is a buffer (or nil, which means
  the current buffer), START and END are buffer positions (integers or
  markers).  If OBJECT is a string, START and END are 0-based indices into it.
  Return t if any property was actually removed, nil otherwise."
  )

(defun next-property-change (position &optional object limit)
  "Return the position of next property change.
  Scans characters forward from POSITION in OBJECT till it finds
  a change in some text property, then returns the position of the change.
  If the optional second argument OBJECT is a buffer (or nil, which means
  the current buffer), POSITION is a buffer position (integer or marker).
  If OBJECT is a string, POSITION is a 0-based index into it.
  Return nil if the property is constant all the way to the end of OBJECT.
  If the value is non-nil, it is a position greater than POSITION, never equal.
  
  If the optional third argument LIMIT is non-nil, don't search
  past position LIMIT; return LIMIT if nothing is found before LIMIT."
  )

(defun text-property-not-all (start end property value &optional object)
  "Check text from START to END for property PROPERTY not equaling VALUE.
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

(defun previous-single-char-property-change (position prop &optional object limit)
  "Return the position of previous text property or overlay change for a specific property.
  Scans characters backward from POSITION till it finds
  a change in the PROP property, then returns the position of the change.
  If the optional third argument OBJECT is a buffer (or nil, which means
  the current buffer), POSITION is a buffer position (integer or marker).
  If OBJECT is a string, POSITION is a 0-based index into it.
  
  In a string, scan runs to the start of the string.
  In a buffer, it runs to (point-min), and the value cannot be less than that.
  
  The property values are compared with `eq'.
  If the property is constant all the way to the start of OBJECT, return the
  first valid position in OBJECT.
  If the optional fourth argument LIMIT is non-nil, don't search back past
  position LIMIT; return LIMIT if nothing is found before reaching LIMIT."
  )

(defun text-property-any (start end property value &optional object)
  "Check text from START to END for property PROPERTY equaling VALUE.
  If so, return the position of the first character whose property PROPERTY
  is `eq' to VALUE.  Otherwise return nil.
  If the optional fifth argument OBJECT is a buffer (or nil, which means
  the current buffer), START and END are buffer positions (integers or
  markers).  If OBJECT is a string, START and END are 0-based indices into it."
  )

(defun get-char-property-and-overlay (position prop &optional object)
  "Like `get-char-property', but with extra overlay information.
  The value is a cons cell.  Its car is the return value of `get-char-property'
  with the same arguments--that is, the value of POSITION's property
  PROP in OBJECT.  Its cdr is the overlay in which the property was
  found, or nil, if it was found as a text property or not found at all.
  
  OBJECT is optional and defaults to the current buffer.  OBJECT may be
  a string, a buffer or a window.  For strings, the cdr of the return
  value is always nil, since strings do not have overlays.  If OBJECT is
  a window, then that window's buffer is used, but window-specific
  overlays are considered only if they are associated with OBJECT.  If
  POSITION is at the end of OBJECT, both car and cdr are nil."
  )

(defun previous-char-property-change (position &optional limit)
  "Return the position of previous text property or overlay change.
  Scans characters backward in the current buffer from POSITION till it
  finds a change in some text property, or the beginning or end of an
  overlay, and returns the position of that.
  If none is found since (point-min), the function returns (point-min).
  
  If the optional second argument LIMIT is non-nil, don't search
  past position LIMIT; return LIMIT if nothing is found before LIMIT.
  LIMIT is a no-op if it is less than (point-min)."
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
  Return t if any property was actually removed, nil otherwise.
  
  Use `set-text-properties' if you want to remove all text properties."
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

(defun next-single-char-property-change (position prop &optional object limit)
  "Return the position of next text property or overlay change for a specific property.
  Scans characters forward from POSITION till it finds
  a change in the PROP property, then returns the position of the change.
  If the optional third argument OBJECT is a buffer (or nil, which means
  the current buffer), POSITION is a buffer position (integer or marker).
  If OBJECT is a string, POSITION is a 0-based index into it.
  
  In a string, scan runs to the end of the string.
  In a buffer, it runs to (point-max), and the value cannot exceed that.
  
  The property values are compared with `eq'.
  If the property is constant all the way to the end of OBJECT, return the
  last valid position in OBJECT.
  If the optional fourth argument LIMIT is non-nil, don't search
  past position LIMIT; return LIMIT if nothing is found before LIMIT."
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
  If the value is non-nil, it is a position greater than POSITION, never equal.
  
  If the optional fourth argument LIMIT is non-nil, don't search
  past position LIMIT; return LIMIT if nothing is found before LIMIT."
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

(defun previous-single-property-change (position prop &optional object limit)
  "Return the position of previous property change for a specific property.
  Scans characters backward from POSITION till it finds
  a change in the PROP property, then returns the position of the change.
  If the optional third argument OBJECT is a buffer (or nil, which means
  the current buffer), POSITION is a buffer position (integer or marker).
  If OBJECT is a string, POSITION is a 0-based index into it.
  The property values are compared with `eq'.
  Return nil if the property is constant all the way to the start of OBJECT.
  If the value is non-nil, it is a position less than POSITION, never equal.
  
  If the optional fourth argument LIMIT is non-nil, don't search
  back past position LIMIT; return LIMIT if nothing is found until LIMIT."
  )

(defun previous-property-change (position &optional object limit)
  "Return the position of previous property change.
  Scans characters backwards from POSITION in OBJECT till it finds
  a change in some text property, then returns the position of the change.
  If the optional second argument OBJECT is a buffer (or nil, which means
  the current buffer), POSITION is a buffer position (integer or marker).
  If OBJECT is a string, POSITION is a 0-based index into it.
  Return nil if the property is constant all the way to the start of OBJECT.
  If the value is non-nil, it is a position less than POSITION, never equal.
  
  If the optional third argument LIMIT is non-nil, don't search
  back past position LIMIT; return LIMIT if nothing is found until LIMIT."
  )

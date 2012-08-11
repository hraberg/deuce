(ns
 deuce.emacs.composite
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude [/]))

(defun find-composition-internal (pos limit string detail-p)
  "Internal use only.
  
  Return information about composition at or nearest to position POS.
  See `find-composition' for more details."
  )

(defun / (dividend divisor &rest divisors)
  "Return first argument divided by all the remaining arguments.
  The arguments must be numbers or markers."
  )

(defun compose-string-internal (string start end &optional components modification-func)
  "Internal use only.
  
  Compose text between indices START and END of STRING.
  Optional 4th and 5th arguments are COMPONENTS and MODIFICATION-FUNC
  for the composition.  See `compose-string' for more details."
  )

(defun compose-region-internal (start end &optional components modification-func)
  "Internal use only.
  
  Compose text in the region between START and END.
  Optional 3rd and 4th arguments are COMPONENTS and MODIFICATION-FUNC
  for the composition.  See `compose-region' for more details."
  )

(defun composition-get-gstring (from to font-object string)
  "Return a glyph-string for characters between FROM and TO.
  If the glyph string is for graphic display, FONT-OBJECT must be
  a font-object to use for those characters.
  Otherwise (for terminal display), FONT-OBJECT must be a terminal ID, a
  frame, or nil for the selected frame's terminal device.
  
  If the optional 4th argument STRING is not nil, it is a string
  containing the target characters between indices FROM and TO.
  
  A glyph-string is a vector containing information about how to display
  a specific character sequence.  The format is:
     [HEADER ID GLYPH ...]
  
  HEADER is a vector of this form:
      [FONT-OBJECT CHAR ...]
  where
      FONT-OBJECT is a font-object for all glyphs in the glyph-string,
      or the terminal coding system of the specified terminal.
      CHARs are characters to be composed by GLYPHs.
  
  ID is an identification number of the glyph-string.  It may be nil if
  not yet shaped.
  
  GLYPH is a vector whose elements have this form:
      [ FROM-IDX TO-IDX C CODE WIDTH LBEARING RBEARING ASCENT DESCENT
        [ [X-OFF Y-OFF WADJUST] | nil] ]
  where
      FROM-IDX and TO-IDX are used internally and should not be touched.
      C is the character of the glyph.
      CODE is the glyph-code of C in FONT-OBJECT.
      WIDTH thru DESCENT are the metrics (in pixels) of the glyph.
      X-OFF and Y-OFF are offsets to the base position for the glyph.
      WADJUST is the adjustment to the normal width of the glyph.
  
  If GLYPH is nil, the remaining elements of the glyph-string vector
  should be ignored."
  )

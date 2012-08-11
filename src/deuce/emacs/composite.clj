(ns
 deuce.emacs.composite
 (:use [deuce.emacs-lisp :only (defun defvar)])
 (:refer-clojure :exclude []))

(defvar compose-chars-after-function nil
  "Function to adjust composition of buffer text.
  
  This function is called with three arguments: FROM, TO, and OBJECT.
  FROM and TO specify the range of text whose composition should be
  adjusted.  OBJECT, if non-nil, is a string that contains the text.
  
  This function is called after a text with `composition' property is
  inserted or deleted to keep `composition' property of buffer text
  valid.
  
  The default value is the function `compose-chars-after'.")

(defvar auto-composition-mode nil
  "Non-nil if Auto-Composition mode is enabled.
  Use the command `auto-composition-mode' to change this variable.")

(defvar auto-composition-function nil
  "Function to call to compose characters automatically.
  This function is called from the display routine with four arguments:
  FROM, TO, WINDOW, and STRING.
  
  If STRING is nil, the function must compose characters in the region
  between FROM and TO in the current buffer.
  
  Otherwise, STRING is a string, and FROM and TO are indices into the
  string.  In this case, the function must compose characters in the
  string.")

(defvar composition-function-table nil
  "Char-table of functions for automatic character composition.
  For each character that has to be composed automatically with
  preceding and/or following characters, this char-table contains
  a function to call to compose that character.
  
  The element at index C in the table, if non-nil, is a list of
  composition rules of this form: ([PATTERN PREV-CHARS FUNC] ...)
  
  PATTERN is a regular expression which C and the surrounding
  characters must match.
  
  PREV-CHARS is a non-negative integer (less than 4) specifying how many
  characters before C to check the matching with PATTERN.  If it is 0,
  PATTERN must match C and the following characters.  If it is 1,
  PATTERN must match a character before C and the following characters.
  
  If PREV-CHARS is 0, PATTERN can be nil, which means that the
  single character C should be composed.
  
  FUNC is a function to return a glyph-string representing a
  composition of the characters that match PATTERN.  It is
  called with one argument GSTRING.
  
  GSTRING is a template of a glyph-string to return.  It is already
  filled with a proper header for the characters to compose, and
  glyphs corresponding to those characters one by one.  The
  function must return a new glyph-string with the same header as
  GSTRING, or modify GSTRING itself and return it.
  
  See also the documentation of `auto-composition-mode'.")

(defun find-composition-internal (pos limit string detail-p)
  "Internal use only.
  
  Return information about composition at or nearest to position POS.
  See `find-composition' for more details."
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

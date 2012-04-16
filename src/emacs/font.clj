(ns emacs.font (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun font-xlfd-name (font &optional fold-wildcards)
  "Return XLFD name of FONT.
  FONT is a font-spec, font-entity, or font-object.
  If the name is too long for XLFD (maximum 255 chars), return nil.
  If the 2nd optional arg FOLD-WILDCARDS is non-nil,
  the consecutive wildcards are folded to one."
  )

(defun font-at (position &optional window string)
  "Return a font-object for displaying a character at POSITION.
  Optional second arg WINDOW, if non-nil, is a window displaying
  the current buffer.  It defaults to the currently selected window."
  )

(defun font-variation-glyphs (font-object character)
  "Return a list of variation glyphs for CHAR in FONT-OBJECT.
  Each element of the value is a cons (VARIATION-SELECTOR . GLYPH-ID),
  where
    VARIATION-SELECTOR is a character code of variation selection
      (#xFE00..#xFE0F or #xE0100..#xE01EF)
    GLYPH-ID is a glyph code of the corresponding variation glyph."
  )

(defun open-font (font-entity &optional size frame)
  "Open FONT-ENTITY.list-processes is an interactive built-in function in `C source code'."
  )

(defun list-fonts (font-spec &optional frame num prefer)
  "List available fonts matching FONT-SPEC on the current frame.
  Optional 2nd argument FRAME specifies the target frame.
  Optional 3rd argument NUM, if non-nil, limits the number of returned fonts.
  Optional 4th argument PREFER, if non-nil, is a font-spec to
  control the order of the returned list.  Fonts are sorted by
  how close they are to PREFER."
  )

(defun font-put (font-spec prop val)
  "Set one property of FONT-SPEC: give property PROP value VAL."
  )

(defun font-spec (&rest args)
  "Return a newly created font-spec with arguments as properties."
  )

(defun get-font-glyphs (font-object string)
  "Return a vector of glyphs of FONT-OBJECT for drawing STRING.
  Each element is a vector [GLYPH-CODE LBEARING RBEARING WIDTH ASCENT DESCENT]."
  )

(defun find-font (font-spec &optional frame)
  "Return a font-entity matching with FONT-SPEC on the current frame.
  Optional 2nd argument FRAME, if non-nil, specifies the target frame.remove-list-of-text-properties is a built-in function in `C source
  code'."
  )

(defun font-shape-gstring (gstring)
  "Shape the glyph-string GSTRING.
  Shaping means substituting glyphs and/or adjusting positions of glyphs
  to get the correct visual image of character sequences set in the
  header of the glyph-string."
  )

(defun query-font (font-object)
  "Return information about FONT-OBJECT.
  The value is a vector:
    [ NAME FILENAME PIXEL-SIZE SIZE ASCENT DESCENT SPACE-WIDTH AVERAGE-WIDTH
      CAPABILITY ]"
  )

(defun font-get (font key)
  "Return the value of FONT's property KEY.
  FONT is a font-spec, a font-entity, or a font-object.
  KEY must be one of these symbols:
    :family, :weight, :slant, :width, :foundry, :adstyle, :registry,
    :size, :name, :script
  See the documentation of `font-spec' for their meanings.
  If FONT is a font-entity or font-object, the value of :script may be
  a list of scripts that are supported by the font."
  )

(defun fontp (object &optional extra-type)
  "Return t if OBJECT is a font-spec, font-entity, or font-object.
  Return nil otherwise.
  Optional 2nd argument EXTRA-TYPE, if non-nil, specifies to check
  which kind of font it is.  It must be one of `font-spec', `font-entity',
  `font-object'."
  )

(defun font-match-p (spec font)
  "Return t if and only if font-spec SPEC matches with FONT.
  FONT is a font-spec, font-entity, or font-object."
  )

(defun font-family-list (&optional frame)
  "List available font families on the current frame.
  Optional argument FRAME, if non-nil, specifies the target frame."
  )

(defun clear-font-cache ()
  "Clear font cache."
  )

(defun close-font (font-object &optional frame)
  "Close FONT-OBJECT."
  )

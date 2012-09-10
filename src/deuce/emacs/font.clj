(ns deuce.emacs.font
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c])
  (:refer-clojure :exclude []))

(defvar font-weight-table nil
  "Vector of valid font weight values.
  Each element has the form:
      [NUMERIC-VALUE SYMBOLIC-NAME ALIAS-NAME ...]
  NUMERIC-VALUE is an integer, and SYMBOLIC-NAME and ALIAS-NAME are symbols.")

(defvar font-encoding-alist nil
  "Alist of fontname patterns vs the corresponding encoding and repertory info.
  Each element looks like (REGEXP . (ENCODING . REPERTORY)),
  where ENCODING is a charset or a char-table,
  and REPERTORY is a charset, a char-table, or nil.

  If ENCODING and REPERTORY are the same, the element can have the form
  (REGEXP . ENCODING).

  ENCODING is for converting a character to a glyph code of the font.
  If ENCODING is a charset, encoding a character by the charset gives
  the corresponding glyph code.  If ENCODING is a char-table, looking up
  the table by a character gives the corresponding glyph code.

  REPERTORY specifies a repertory of characters supported by the font.
  If REPERTORY is a charset, all characters belonging to the charset are
  supported.  If REPERTORY is a char-table, all characters who have a
  non-nil value in the table are supported.  If REPERTORY is nil, Emacs
  gets the repertory information by an opened font and ENCODING.")

(defvar font-log nil
  "*Logging list of font related actions and results.
  The value t means to suppress the logging.
  The initial value is set to nil if the environment variable
  EMACS_FONT_LOG is set.  Otherwise, it is set to t.")

(defvar font-width-table nil
  "Alist of font width symbols vs the corresponding numeric values.
  See `font-weight-table' for the format of the vector.")

(defvar font-slant-table nil
  "Vector of font slant symbols vs the corresponding numeric values.
  See `font-weight-table' for the format of the vector.")

(defun font-xlfd-name (font &optional fold-wildcards)
  "Return XLFD name of FONT.
  FONT is a font-spec, font-entity, or font-object.
  If the name is too long for XLFD (maximum 255 chars), return nil.
  If the 2nd optional arg FOLD-WILDCARDS is non-nil,
  the consecutive wildcards are folded into one."
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
  "Open FONT-ENTITY."
  )

(defun list-fonts (font-spec &optional frame num prefer)
  "List available fonts matching FONT-SPEC on the current frame.
  Optional 2nd argument FRAME specifies the target frame.
  Optional 3rd argument NUM, if non-nil, limits the number of returned fonts.
  Optional 4th argument PREFER, if non-nil, is a font-spec to
  control the order of the returned list.  Fonts are sorted by
  how close they are to PREFER."
  )

(defun font-get-glyphs (font-object from to &optional object)
  "Return a vector of FONT-OBJECT's glyphs for the specified characters.
  FROM and TO are positions (integers or markers) specifying a region
  of the current buffer.
  If the optional fourth arg OBJECT is not nil, it is a string or a
  vector containing the target characters.

  Each element is a vector containing information of a glyph in this format:
    [FROM-IDX TO-IDX C CODE WIDTH LBEARING RBEARING ASCENT DESCENT ADJUSTMENT]
  where
    FROM is an index numbers of a character the glyph corresponds to.
    TO is the same as FROM.
    C is the character of the glyph.
    CODE is the glyph-code of C in FONT-OBJECT.
    WIDTH thru DESCENT are the metrics (in pixels) of the glyph.
    ADJUSTMENT is always nil.
  If FONT-OBJECT doesn't have a glyph for a character,
  the corresponding element is nil."
  )

(defun font-put (font prop val)
  "Set one property of FONT: give property KEY value VAL.
  FONT is a font-spec, a font-entity, or a font-object.

  If FONT is a font-spec, KEY can be any symbol.  But if KEY is the one
  accepted by the function `font-spec' (which see), VAL must be what
  allowed in `font-spec'.

  If FONT is a font-entity or a font-object, KEY must not be the one
  accepted by `font-spec'."
  )

(defun font-spec (&rest args)
  "Return a newly created font-spec with arguments as properties.

  ARGS must come in pairs KEY VALUE of font properties.  KEY must be a
  valid font property name listed below:

  `:family', `:weight', `:slant', `:width'

  They are the same as face attributes of the same name.  See
  `set-face-attribute'.

  `:foundry'

  VALUE must be a string or a symbol specifying the font foundry, e.g. ``misc''.

  `:adstyle'

  VALUE must be a string or a symbol specifying the additional
  typographic style information of a font, e.g. ``sans''.

  `:registry'

  VALUE must be a string or a symbol specifying the charset registry and
  encoding of a font, e.g. ``iso8859-1''.

  `:size'

  VALUE must be a non-negative integer or a floating point number
  specifying the font size.  It specifies the font size in pixels (if
  VALUE is an integer), or in points (if VALUE is a float).

  `:name'

  VALUE must be a string of XLFD-style or fontconfig-style font name.

  `:script'

  VALUE must be a symbol representing a script that the font must
  support.  It may be a symbol representing a subgroup of a script
  listed in the variable `script-representative-chars'.

  `:lang'

  VALUE must be a symbol of two-letter ISO-639 language names,
  e.g. `ja'.

  `:otf'

  VALUE must be a list (SCRIPT-TAG LANGSYS-TAG GSUB [ GPOS ]) to specify
  required OpenType features.

    SCRIPT-TAG: OpenType script tag symbol (e.g. `deva').
    LANGSYS-TAG: OpenType language system tag symbol,
       or nil for the default language system.
    GSUB: List of OpenType GSUB feature tag symbols, or nil if none required.
    GPOS: List of OpenType GPOS feature tag symbols, or nil if none required.

  GSUB and GPOS may contain `nil' element.  In such a case, the font
  must not have any of the remaining elements.

  For instance, if the VALUE is `(thai nil nil (mark))', the font must
  be an OpenType font whose GPOS table of `thai' script's default
  language system must contain `mark' feature."
  )

(defun find-font (font-spec &optional frame)
  "Return a font-entity matching with FONT-SPEC on the current frame.
  Optional 2nd argument FRAME, if non-nil, specifies the target frame."
  )

(defun font-shape-gstring (gstring)
  "Shape the glyph-string GSTRING.
  Shaping means substituting glyphs and/or adjusting positions of glyphs
  to get the correct visual image of character sequences set in the
  header of the glyph-string.

  If the shaping was successful, the value is GSTRING itself or a newly
  created glyph-string.  Otherwise, the value is nil."
  )

(defun query-font (font-object)
  "Return information about FONT-OBJECT.
  The value is a vector:
    [ NAME FILENAME PIXEL-SIZE SIZE ASCENT DESCENT SPACE-WIDTH AVERAGE-WIDTH
      CAPABILITY ]

  NAME is the font name, a string (or nil if the font backend doesn't
  provide a name).

  FILENAME is the font file name, a string (or nil if the font backend
  doesn't provide a file name).

  PIXEL-SIZE is a pixel size by which the font is opened.

  SIZE is a maximum advance width of the font in pixels.

  ASCENT, DESCENT, SPACE-WIDTH, AVERAGE-WIDTH are metrics of the font in
  pixels.

  CAPABILITY is a list whose first element is a symbol representing the
  font format (x, opentype, truetype, type1, pcf, or bdf) and the
  remaining elements describe the details of the font capability.

  If the font is OpenType font, the form of the list is
    (opentype GSUB GPOS)
  where GSUB shows which \"GSUB\" features the font supports, and GPOS
  shows which \"GPOS\" features the font supports.  Both GSUB and GPOS are
  lists of the format:
    ((SCRIPT (LANGSYS FEATURE ...) ...) ...)

  If the font is not OpenType font, currently the length of the form is
  one.

  SCRIPT is a symbol representing OpenType script tag.

  LANGSYS is a symbol representing OpenType langsys tag, or nil
  representing the default langsys.

  FEATURE is a symbol representing OpenType feature tag.

  If the font is not OpenType font, CAPABILITY is nil."
  )

(defun font-get (font key)
  "Return the value of FONT's property KEY.
  FONT is a font-spec, a font-entity, or a font-object.
  KEY is any symbol, but these are reserved for specific meanings:
    :family, :weight, :slant, :width, :foundry, :adstyle, :registry,
    :size, :name, :script, :otf
  See the documentation of `font-spec' for their meanings.
  In addition, if FONT is a font-entity or a font-object, values of
  :script and :otf are different from those of a font-spec as below:

  The value of :script may be a list of scripts that are supported by the font.

  The value of :otf is a cons (GSUB . GPOS) where GSUB and GPOS are lists
  representing the OpenType features supported by the font by this form:
    ((SCRIPT (LANGSYS FEATURE ...) ...) ...)
  SCRIPT, LANGSYS, and FEATURE are all symbols representing OpenType
  Layout tags."
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

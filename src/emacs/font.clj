(ns emacs.font (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun font-xlfd-name (font &optional fold-wildcards)
  "Return XLFD name of FONT.\nFONT is a font-spec, font-entity, or font-object.\nIf the name is too long for XLFD (maximum 255 chars), return nil.\nIf the 2nd optional arg FOLD-WILDCARDS is non-nil,\n"
  )

(defun font-at (position &optional window string)
  "Return a font-object for displaying a character at POSITION.\nOptional second arg WINDOW, if non-nil, is a window displaying\n"
  )

(defun font-variation-glyphs (font-object character)
  "Return a list of variation glyphs for CHAR in FONT-OBJECT.\nEach element of the value is a cons (VARIATION-SELECTOR . GLYPH-ID),\nwhere\n  VARIATION-SELECTOR is a character code of variation selection\n    (#xFE00..#xFE0F or #xE0100..#xE01EF)\n"
  )

(defun open-font (font-entity &optional size frame)
  "Open FONT-ENTITY.list-processes is an interactive built-in function in `C source code'."
  )

(defun list-fonts (font-spec &optional frame num prefer)
  "List available fonts matching FONT-SPEC on the current frame.\nOptional 2nd argument FRAME specifies the target frame.\nOptional 3rd argument NUM, if non-nil, limits the number of returned fonts.\nOptional 4th argument PREFER, if non-nil, is a font-spec to\ncontrol the order of the returned list.  Fonts are sorted by\n"
  )

(defun font-put (font-spec prop val)
  )

(defun font-spec (args-dot-dot-dot)
  "Return a newly created font-spec with arguments as properties."
  )

(defun get-font-glyphs (font-object string)
  "Return a vector of glyphs of FONT-OBJECT for drawing STRING.\n"
  )

(defun find-font (font-spec &optional frame)
  "Return a font-entity matching with FONT-SPEC on the current frame.\nOptional 2nd argument FRAME, if non-nil, specifies the target frame.remove-list-of-text-properties is a built-in function in `C source\ncode'."
  )

(defun font-shape-gstring (gstring)
  "Shape the glyph-string GSTRING.\nShaping means substituting glyphs and/or adjusting positions of glyphs\nto get the correct visual image of character sequences set in the\nheader of the glyph-string."
  )

(defun query-font (font-object)
  "Return information about FONT-OBJECT.\nThe value is a vector:\n  [ NAME FILENAME PIXEL-SIZE SIZE ASCENT DESCENT SPACE-WIDTH AVERAGE-WIDTH\n    CAPABILITY ]"
  )

(defun font-get (font key)
  "Return the value of FONT's property KEY.\nFONT is a font-spec, a font-entity, or a font-object.\nKEY must be one of these symbols:\n  :family, :weight, :slant, :width, :foundry, :adstyle, :registry,\n  :size, :name, :script\nSee the documentation of `font-spec' for their meanings.\nIf FONT is a font-entity or font-object, the value of :script may be\n"
  )

(defun fontp (object &optional extra-type)
  "Return t if OBJECT is a font-spec, font-entity, or font-object.\nReturn nil otherwise.\nOptional 2nd argument EXTRA-TYPE, if non-nil, specifies to check\nwhich kind of font it is.  It must be one of `font-spec', `font-entity',\n"
  )

(defun font-match-p (spec font)
  "Return t if and only if font-spec SPEC matches with FONT.\n"
  )

(defun font-family-list (&optional frame)
  "List available font families on the current frame.\n"
  )

(defun clear-font-cache ()
  )

(defun close-font (font-object &optional frame)
  )

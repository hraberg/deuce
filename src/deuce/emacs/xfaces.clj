(ns
 deuce.emacs.xfaces
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun internal-make-lisp-face (face &optional frame)
  "Make FACE, a symbol, a Lisp face with all attributes nil.
  If FACE was not known as a face before, create a new one.
  If optional argument FRAME is specified, make a frame-local face
  for that frame.  Otherwise operate on the global face definition.
  Value is a vector of face attributes."
  )

(defun display-supports-face-attributes-p (attributes &optional display)
  "Return non-nil if all the face attributes in ATTRIBUTES are supported.
  The optional argument DISPLAY can be a display name, a frame, or
  nil (meaning the selected frame's display).
  
  The definition of `supported' is somewhat heuristic, but basically means
  that a face containing all the attributes in ATTRIBUTES, when merged
  with the default face for display, can be represented in a way that's
  
   (1) different in appearance than the default face, and
   (2) `close in spirit' to what the attributes specify, if not exact.
  
  Point (2) implies that a `:weight black' attribute will be satisfied by
  any display that can display bold, and a `:foreground \"yellow\"' as long
  as it can display a yellowish color, but `:slant italic' will _not_ be
  satisfied by the tty display code's automatic substitution of a `dim'
  face for italic."
  )

(defun face-attribute-relative-p (attribute value)
  "Check whether a face attribute value is relative.
  Specifically, this function returns t if the attribute ATTRIBUTE
  with the value VALUE is relative.
  
  A relative value is one that doesn't entirely override whatever is
  inherited from another face.  For most possible attributes,
  the only relative value that users see is `unspecified'.
  However, for :height, floating point values are also relative."
  )

(defun color-supported-p (color &optional frame background-p)
  "Return non-nil if COLOR can be displayed on FRAME.
  BACKGROUND-P non-nil means COLOR is used as a background.
  Otherwise, this function tells whether it can be used as a foreground.
  If FRAME is nil or omitted, use the selected frame.
  COLOR must be a valid color name."
  )

(defun x-load-color-file (filename)
  "Create an alist of color entries from an external file.
  
  The file should define one named RGB color per line like so:
    R G B   name
  where R,G,B are numbers between 0 and 255 and name is an arbitrary string."
  )

(defun internal-lisp-face-empty-p (face &optional frame)
  "True if FACE has no attribute specified.
  If the optional argument FRAME is given, report on face FACE in that frame.
  If FRAME is t, report on the defaults for face FACE (for new frames).
  If FRAME is omitted or nil, use the selected frame."
  )

(defun internal-set-lisp-face-attribute (face attr value &optional frame)
  "Set attribute ATTR of FACE to VALUE.
  FRAME being a frame means change the face on that frame.
  FRAME nil means change the face of the selected frame.
  FRAME t means change the default for new frames.
  FRAME 0 means change the face on all frames, and change the default
    for new frames."
  )

(defun clear-face-cache (&optional thoroughly)
  "Clear face caches on all frames.
  Optional THOROUGHLY non-nil means try to free unused fonts, too."
  )

(defun internal-set-alternative-font-registry-alist (alist)
  "Define alternative font registries to try in face font selection.
  ALIST is an alist of (REGISTRY ALTERNATIVE1 ALTERNATIVE2 ...) entries.
  Each ALTERNATIVE is tried in order if no fonts of font registry REGISTRY can
  be found.  Value is ALIST."
  )

(defun merge-face-attribute (attribute value1 value2)
  "Return face ATTRIBUTE VALUE1 merged with VALUE2.
  If VALUE1 or VALUE2 are absolute (see `face-attribute-relative-p'), then
  the result will be absolute, otherwise it will be relative."
  )

(defun color-gray-p (color &optional frame)
  "Return non-nil if COLOR is a shade of gray (or white or black).
  FRAME specifies the frame and thus the display for interpreting COLOR.
  If FRAME is nil or omitted, use the selected frame."
  )

(defun tty-suppress-bold-inverse-default-colors (suppress)
  "Suppress/allow boldness of faces with inverse default colors.
  SUPPRESS non-nil means suppress it.
  This affects bold faces on TTYs whose foreground is the default background
  color of the display and whose background is the default foreground color.
  For such faces, the bold face attribute is ignored if this variable
  is non-nil."
  )

(defun internal-merge-in-global-face (face frame)
  "Add attributes from frame-default definition of FACE to FACE on FRAME.
  Default face attributes override any local face attributes."
  )

(defun face-font (face &optional frame character)
  "Return the font name of face FACE, or nil if it is unspecified.
  The font name is, by default, for ASCII characters.
  If the optional argument FRAME is given, report on face FACE in that frame.
  If FRAME is t, report on the defaults for face FACE (for new frames).
    The font default for a face is either nil, or a list
    of the form (bold), (italic) or (bold italic).
  If FRAME is omitted or nil, use the selected frame.  And, in this case,
  if the optional third argument CHARACTER is given,
  return the font name used for CHARACTER."
  )

(defun internal-lisp-face-attribute-values (attr)
  "Return a list of valid discrete values for face attribute ATTR.
  Value is nil if ATTR doesn't have a discrete set of valid values."
  )

(defun internal-lisp-face-equal-p (face1 face2 &optional frame)
  "True if FACE1 and FACE2 are equal.
  If the optional argument FRAME is given, report on FACE1 and FACE2 in that frame.
  If FRAME is t, report on the defaults for FACE1 and FACE2 (for new frames).
  If FRAME is omitted or nil, use the selected frame."
  )

(defun internal-set-font-selection-order (order)
  "Set font selection order for face font selection to ORDER.
  ORDER must be a list of length 4 containing the symbols `:width',
  `:height', `:weight', and `:slant'.  Face attributes appearing
  first in ORDER are matched first, e.g. if `:height' appears before
  `:weight' in ORDER, font selection first tries to find a font with
  a suitable height, and then tries to match the font weight.
  Value is ORDER."
  )

(defun internal-lisp-face-p (face &optional frame)
  "Return non-nil if FACE names a face.
  FACE should be a symbol or string.
  If optional second argument FRAME is non-nil, check for the
  existence of a frame-local face with name FACE on that frame.
  Otherwise check for the existence of a global face."
  )

(defun internal-copy-lisp-face (from to frame new-frame)
  "Copy face FROM to TO.
  If FRAME is t, copy the global face definition of FROM.
  Otherwise, copy the frame-local definition of FROM on FRAME.
  If NEW-FRAME is a frame, copy that data into the frame-local
  definition of TO on NEW-FRAME.  If NEW-FRAME is nil,
  FRAME controls where the data is copied to.
  
  The value is TO."
  )

(defun frame-face-alist (&optional frame)
  "Return an alist of frame-local faces defined on FRAME.
  For internal use only."
  )

(defun internal-set-alternative-font-family-alist (alist)
  "Define alternative font families to try in face font selection.
  ALIST is an alist of (FAMILY ALTERNATIVE1 ALTERNATIVE2 ...) entries.
  Each ALTERNATIVE is tried in order if no fonts of font family FAMILY can
  be found.  Value is ALIST."
  )

(defun face-attributes-as-vector (plist)
  "Return a vector of face attributes corresponding to PLIST."
  )

(defun internal-get-lisp-face-attribute (symbol keyword &optional frame)
  "Return face attribute KEYWORD of face SYMBOL.
  If SYMBOL does not name a valid Lisp face or KEYWORD isn't a valid
  face attribute name, signal an error.
  If the optional argument FRAME is given, report on face SYMBOL in that
  frame.  If FRAME is t, report on the defaults for face SYMBOL (for new
  frames).  If FRAME is omitted or nil, use the selected frame."
  )

(defun color-distance (color1 color2 &optional frame)
  "Return an integer distance between COLOR1 and COLOR2 on FRAME.
  COLOR1 and COLOR2 may be either strings containing the color name,
  or lists of the form (RED GREEN BLUE).
  If FRAME is unspecified or nil, the current frame is used."
  )

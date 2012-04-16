(ns emacs.xfaces (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun internal-make-lisp-face (face &optional frame)
  "Make FACE, a symbol, a Lisp face with all attributes nil.
  If FACE was not known as a face before, create a new one.
  If optional argument FRAME is specified, make a frame-local face
  for that frame.  Otherwise operate on the global face definition.
  Value is a vector of face attributes."
  )

(defun face-attribute-relative-p (attribute value)
  "Check whether a face attribute value is relative.
  Specifically, this function returns t if the attribute ATTRIBUTE
  with the value VALUE is relative."
  )

(defun color-supported-p (color &optional frame background-p)
  "Return non-nil if COLOR can be displayed on FRAME.
  BACKGROUND-P non-nil means COLOR is used as a background.
  Otherwise, this function tells whether it can be used as a foreground.
  If FRAME is nil or omitted, use the selected frame.
  COLOR must be a valid color name."
  )

(defun x-load-color-file (filename)
  "Create an alist of color entries from an external file."
  )

(defun internal-lisp-face-empty-p (face &optional frame)
  "True if FACE has no attribute specified.
  If the optional argument FRAME is given, report on face FACE in that frame.
  If FRAME is t, report on the defaults for face FACE (for new frames).
  If FRAME is omitted or nil, use the selected frame."
  )

(defun clear-face-cache (&optional thoroughly)
  "Clear face caches on all frames.
  Optional THOROUGHLY non-nil means try to free unused fonts, too."
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

(defun internal-lisp-face-equal-p (face1 face2 &optional frame)
  "True if FACE1 and FACE2 are equal.
  If the optional argument FRAME is given, report on FACE1 and FACE2 in that frame.
  If FRAME is t, report on the defaults for FACE1 and FACE2 (for new frames).
  If FRAME is omitted or nil, use the selected frame."
  )

(defun internal-lisp-face-p (face &optional frame)
  "Return non-nil if FACE names a face.
  FACE should be a symbol or string.
  If optional second argument FRAME is non-nil, check for the
  existence of a frame-local face with name FACE on that frame.
  Otherwise check for the existence of a global face.upcase-region is an interactive built-in function in `C source code'."
  )

(defun internal-copy-lisp-face (from to frame new-frame)
  "Copy face FROM to TO.
  If FRAME is t, copy the global face definition of FROM.
  Otherwise, copy the frame-local definition of FROM on FRAME.
  If NEW-FRAME is a frame, copy that data into the frame-local
  definition of TO on NEW-FRAME.  If NEW-FRAME is nil,
  FRAME controls where the data is copied to."
  )

(defun frame-face-alist (&optional frame)
  "Return an alist of frame-local faces defined on FRAME.
  For internal use only."
  )

(defun face-attributes-as-vector (plist)
  "Return a vector of face attributes corresponding to PLIST."
  )

(defun color-distance (color1 color2 &optional frame)
  "Return an integer distance between COLOR1 and COLOR2 on FRAME.
  COLOR1 and COLOR2 may be either strings containing the color name,
  or lists of the form (RED GREEN BLUE).
  If FRAME is unspecified or nil, the current frame is used."
  )

(ns emacs.xfaces (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun internal-make-lisp-face (face &optional frame)
  "Make FACE, a symbol, a Lisp face with all attributes nil.\nIf FACE was not known as a face before, create a new one.\nIf optional argument FRAME is specified, make a frame-local face\nfor that frame.  Otherwise operate on the global face definition.\n"
  )

(defun face-attribute-relative-p (attribute value)
  "Check whether a face attribute value is relative.\nSpecifically, this function returns t if the attribute ATTRIBUTE\nwith the value VALUE is relative."
  )

(defun color-supported-p (color &optional frame background-p)
  "Return non-nil if COLOR can be displayed on FRAME.\nBACKGROUND-P non-nil means COLOR is used as a background.\nOtherwise, this function tells whether it can be used as a foreground.\nIf FRAME is nil or omitted, use the selected frame.\n"
  )

(defun x-load-color-file (filename)
  "Create an alist of color entries from an external file."
  )

(defun internal-lisp-face-empty-p (face &optional frame)
  "True if FACE has no attribute specified.\nIf the optional argument FRAME is given, report on face FACE in that frame.\nIf FRAME is t, report on the defaults for face FACE (for new frames).\n"
  )

(defun clear-face-cache (&optional thoroughly)
  "Clear face caches on all frames.\n"
  )

(defun merge-face-attribute (attribute value1 value2)
  "Return face ATTRIBUTE VALUE1 merged with VALUE2.\nIf VALUE1 or VALUE2 are absolute (see `face-attribute-relative-p'), then\n"
  )

(defun color-gray-p (color &optional frame)
  "Return non-nil if COLOR is a shade of gray (or white or black).\nFRAME specifies the frame and thus the display for interpreting COLOR.\n"
  )

(defun face-font (face &optional frame character)
  "Return the font name of face FACE, or nil if it is unspecified.\nThe font name is, by default, for ASCII characters.\nIf the optional argument FRAME is given, report on face FACE in that frame.\nIf FRAME is t, report on the defaults for face FACE (for new frames).\n  The font default for a face is either nil, or a list\n  of the form (bold), (italic) or (bold italic).\nIf FRAME is omitted or nil, use the selected frame.  And, in this case,\nif the optional third argument CHARACTER is given,\n"
  )

(defun internal-lisp-face-equal-p (face1 face2 &optional frame)
  "True if FACE1 and FACE2 are equal.\nIf the optional argument FRAME is given, report on FACE1 and FACE2 in that frame.\nIf FRAME is t, report on the defaults for FACE1 and FACE2 (for new frames).\n"
  )

(defun internal-lisp-face-p (face &optional frame)
  "Return non-nil if FACE names a face.\nFACE should be a symbol or string.\nIf optional second argument FRAME is non-nil, check for the\nexistence of a frame-local face with name FACE on that frame.\nOtherwise check for the existence of a global face.upcase-region is an interactive built-in function in `C source code'."
  )

(defun internal-copy-lisp-face (from to frame new-frame)
  "Copy face FROM to TO.\nIf FRAME is t, copy the global face definition of FROM.\nOtherwise, copy the frame-local definition of FROM on FRAME.\nIf NEW-FRAME is a frame, copy that data into the frame-local\ndefinition of TO on NEW-FRAME.  If NEW-FRAME is nil,\nFRAME controls where the data is copied to."
  )

(defun frame-face-alist (&optional frame)
  "Return an alist of frame-local faces defined on FRAME.\n"
  )

(defun face-attributes-as-vector (plist)
  )

(defun color-distance (color1 color2 &optional frame)
  "Return an integer distance between COLOR1 and COLOR2 on FRAME.\nCOLOR1 and COLOR2 may be either strings containing the color name,\nor lists of the form (RED GREEN BLUE).\n"
  )

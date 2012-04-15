(ns emacs.composite (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun find-composition-internal (pos limit string detail-p)
  "Internal use only."
  )

(defun compose-string-internal (string start end &optional components modification-func)
  "Internal use only."
  )

(defun compose-region-internal (start end &optional components modification-func)
  "Internal use only."
  )

(defun composition-get-gstring (from to font-object string)
  "Return a glyph-string for characters between FROM and TO.\nIf the glyph string is for graphic display, FONT-OBJECT must be\na font-object to use for those characters.\nOtherwise (for terminal display), FONT-OBJECT must be a terminal ID, a\nframe, or nil for the selected frame's terminal device."
  )

(ns emacs.menu (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun x-popup-menu (position menu)
  "Pop up a deck-of-cards menu and return user's selection.
  POSITION is a position specification.  This is either a mouse button event
  or a list ((XOFFSET YOFFSET) WINDOW)
  where XOFFSET and YOFFSET are positions in pixels from the top left
  corner of WINDOW.  (WINDOW may be a window or a frame object.)
  This controls the position of the top left of the menu as a whole.
  If POSITION is t, it means to use the current mouse position."
  )

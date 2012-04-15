(ns emacs.menu (use [deuce.core]) (:refer-clojure :only []))

(defun x-popup-menu (position menu)
  "Pop up a deck-of-cards menu and return user's selection.\nPOSITION is a position specification.  This is either a mouse button event\nor a list ((XOFFSET YOFFSET) WINDOW)\nwhere XOFFSET and YOFFSET are positions in pixels from the top left\ncorner of WINDOW.  (WINDOW may be a window or a frame object.)\nThis controls the position of the top left of the menu as a whole.\nIf POSITION is t, it means to use the current mouse position."
  )

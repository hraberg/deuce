(ns emacs.indent (use [deuce.core]) (:refer-clojure :only []))

(defun current-indentation ()
  "Return the indentation of the current line.\nThis is the horizontal position of the character\n"
  )

(defun current-column ()
  "Return the horizontal position of point.  Beginning of line is column 0.\nThis is calculated by adding together the widths of all the displayed\nrepresentations of the character between the start of the previous line\nand point (eg. control characters will have a width of 2 or 4, tabs\nwill have a variable width).\nIgnores finite width of frame, which means that this function may return\nvalues greater than (frame-width).\nWhether the line is visible (if `selective-display' is t) has no effect;\nhowever, ^M is treated as end of line when `selective-display' is t.\nText that has an invisible property is considered as having width 0, unless\n"
  )

(defun compute-motion (from frompos to topos width offsets window)
  "Scan through the current buffer, calculating screen position.\nScan the current buffer forward from offset FROM,\nassuming it is at position FROMPOS--a cons of the form (HPOS . VPOS)--\nto position TO or position TOPOS--another cons of the form (HPOS . VPOS)--\nand return the ending buffer position and screen location."
  )

(defun vertical-motion (lines &optional window)
  "Move point to start of the screen line LINES lines down.\nIf LINES is negative, this means moving up."
  )

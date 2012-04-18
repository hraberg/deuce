(ns
 deuce.emacs.indent
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun current-indentation ()
  "Return the indentation of the current line.
  This is the horizontal position of the character
  following any initial whitespace."
  )

(defun current-column ()
  "Return the horizontal position of point.  Beginning of line is column 0.
  This is calculated by adding together the widths of all the displayed
  representations of the character between the start of the previous line
  and point (eg. control characters will have a width of 2 or 4, tabs
  will have a variable width).
  Ignores finite width of frame, which means that this function may return
  values greater than (frame-width).
  Whether the line is visible (if `selective-display' is t) has no effect;
  however, ^M is treated as end of line when `selective-display' is t.
  Text that has an invisible property is considered as having width 0, unless
  `buffer-invisibility-spec' specifies that it is replaced by an ellipsis."
  )

(defun compute-motion (from frompos to topos width offsets window)
  "Scan through the current buffer, calculating screen position.
  Scan the current buffer forward from offset FROM,
  assuming it is at position FROMPOS--a cons of the form (HPOS . VPOS)--
  to position TO or position TOPOS--another cons of the form (HPOS . VPOS)--
  and return the ending buffer position and screen location."
  )

(defun indent-to (column &optional minimum)
  "Indent from point with tabs and spaces until COLUMN is reached.
  Optional second argument MINIMUM says always do at least MINIMUM spaces
  even if that goes past COLUMN; by default, MINIMUM is zero."
  )

(defun move-to-column (column &optional force)
  "Move point to column COLUMN in the current line.
  Interactively, COLUMN is the value of prefix numeric argument.
  The column of a character is calculated by adding together the widths
  as displayed of the previous characters in the line.
  This function ignores line-continuation;
  there is no upper limit on the column number a character can have
  and horizontal scrolling has no effect."
  )

(defun vertical-motion (lines &optional window)
  "Move point to start of the screen line LINES lines down.
  If LINES is negative, this means moving up."
  )

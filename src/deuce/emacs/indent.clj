(ns
 deuce.emacs.indent
 (use [deuce.emacs-lisp :only (defun defvar)])
 (require [clojure.core :as c])
 (:refer-clojure :exclude []))

(defvar indent-tabs-mode nil
  "*Indentation can insert tabs if this is non-nil.
  
  You can customize this variable.")

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
  and return the ending buffer position and screen location.
  
  If TOPOS is nil, the actual width and height of the window's
  text area are used.
  
  There are three additional arguments:
  
  WIDTH is the number of columns available to display text;
  this affects handling of continuation lines.  A value of nil
  corresponds to the actual number of available text columns.
  
  OFFSETS is either nil or a cons cell (HSCROLL . TAB-OFFSET).
  HSCROLL is the number of columns not being displayed at the left
  margin; this is usually taken from a window's hscroll member.
  TAB-OFFSET is the number of columns of the first tab that aren't
  being displayed, perhaps because the line was continued within it.
  If OFFSETS is nil, HSCROLL and TAB-OFFSET are assumed to be zero.
  
  WINDOW is the window to operate on.  It is used to choose the display table;
  if it is showing the current buffer, it is used also for
  deciding which overlay properties apply.
  Note that `compute-motion' always operates on the current buffer.
  
  The value is a list of five elements:
    (POS HPOS VPOS PREVHPOS CONTIN)
  POS is the buffer position where the scan stopped.
  VPOS is the vertical position where the scan stopped.
  HPOS is the horizontal position where the scan stopped.
  
  PREVHPOS is the horizontal position one character back from POS.
  CONTIN is t if a line was continued after (or within) the previous character.
  
  For example, to find the buffer position of column COL of line LINE
  of a certain window, pass the window's starting location as FROM
  and the window's upper-left coordinates as FROMPOS.
  Pass the buffer's (point-max) as TO, to limit the scan to the end of the
  visible section of the buffer, and pass LINE and COL as TOPOS."
  )

(defun indent-to (column &optional minimum)
  "Indent from point with tabs and spaces until COLUMN is reached.
  Optional second argument MINIMUM says always do at least MINIMUM spaces
  even if that goes past COLUMN; by default, MINIMUM is zero.
  
  The return value is COLUMN."
  )

(defun move-to-column (column &optional force)
  "Move point to column COLUMN in the current line.
  Interactively, COLUMN is the value of prefix numeric argument.
  The column of a character is calculated by adding together the widths
  as displayed of the previous characters in the line.
  This function ignores line-continuation;
  there is no upper limit on the column number a character can have
  and horizontal scrolling has no effect.
  
  If specified column is within a character, point goes after that character.
  If it's past end of line, point goes to end of line.
  
  Optional second argument FORCE non-nil means if COLUMN is in the
  middle of a tab character, change it to spaces.
  In addition, if FORCE is t, and the line is too short to reach
  COLUMN, add spaces/tabs to get there.
  
  The return value is the current column."
  )

(defun vertical-motion (lines &optional window)
  "Move point to start of the screen line LINES lines down.
  If LINES is negative, this means moving up.
  
  This function is an ordinary cursor motion function
  which calculates the new position based on how text would be displayed.
  The new position may be the start of a line,
  or just the start of a continuation line.
  The function returns number of screen lines moved over;
  that usually equals LINES, but may be closer to zero
  if beginning or end of buffer was reached.
  
  The optional second argument WINDOW specifies the window to use for
  parameters such as width, horizontal scrolling, and so on.
  The default is to use the selected window's parameters.
  
  LINES can optionally take the form (COLS . LINES), in which case
  the motion will not stop at the start of a screen line but on
  its column COLS (if such exists on that line, that is).
  
  `vertical-motion' always uses the current buffer,
  regardless of which buffer is displayed in WINDOW.
  This is consistent with other cursor motion functions
  and makes it possible to use `vertical-motion' in any buffer,
  whether or not it is currently displayed in some window."
  )

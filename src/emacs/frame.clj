(ns emacs.frame (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun frame-parameters (&optional frame)
  "Return the parameters-alist of frame FRAME.\nIt is a list of elements of the form (PARM . VALUE), where PARM is a symbol.\nThe meaningful PARMs depend on the kind of frame.\n"
  )

(defun frame-parameter (frame parameter)
  "Return FRAME's value for parameter PARAMETER.\nIf FRAME is nil, describe the currently selected frame.make-local-variable is an interactive built-in function in `C source\ncode'."
  )

(defun framep (object)
  "Return non-nil if OBJECT is a frame.\nValue is:\n  t for a termcap frame (a character-only terminal),\n 'x' for an Emacs frame that is really an X window,\n 'w32' for an Emacs frame that is a window on MS-Windows display,\n 'ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,\n 'pc' for a direct-write MS-DOS frame.\n"
  )

(defun frame-visible-p (frame)
  "Return t if FRAME is \"visible\" (actually in use for display).\nReturn the symbol `icon' if FRAME is iconified or \"minimized\".\nReturn nil if FRAME was made invisible, via `make-frame-invisible'.\nOn graphical displays, invisible frames are not updated and are\nusually not displayed at all, even in a window system's \"taskbar\"."
  )

(defun make-terminal-frame (parms)
  "Create an additional terminal frame, possibly on another terminal.\nThis function takes one argument, an alist specifying frame parameters."
  )

(defun modify-frame-parameters (frame alist)
  "Modify the parameters of frame FRAME according to ALIST.\nIf FRAME is nil, it defaults to the selected frame.\nALIST is an alist of parameters to change and their new values.\nEach element of ALIST has the form (PARM . VALUE), where PARM is a symbol.\nThe meaningful PARMs depend on the kind of frame.\nUndefined PARMs are ignored, but stored in the frame's parameter list\nso that `frame-parameters' will return them."
  )

(defun frame-list ()
  )

(defun set-frame-size (frame cols rows)
  "Sets size of FRAME to COLS by ROWS, measured in characters.set-window-redisplay-end-trigger is a built-in function in `C source\ncode'."
  )

(defun window-system (&optional frame)
  "The name of the window system that FRAME is displaying through.\nThe value is a symbol:\n nil for a termcap frame (a character-only terminal),\n 'x' for an Emacs frame that is really an X window,\n 'w32' for an Emacs frame that is a window on MS-Windows display,\n 'ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,\n 'pc' for a direct-write MS-DOS frame."
  )

(defun previous-frame (&optional frame miniframe)
  "Return the previous frame in the frame list before FRAME.\nIt considers only frames on the same terminal as FRAME.\nBy default, skip minibuffer-only frames.\nIf omitted, FRAME defaults to the selected frame.\nIf optional argument MINIFRAME is nil, exclude minibuffer-only frames.\nIf MINIFRAME is a window, include only its own frame\nand any frame now using that window as the minibuffer.\nIf MINIFRAME is `visible', include all visible frames.\nIf MINIFRAME is 0, include all visible and iconified frames.\n"
  )

(defun set-mouse-pixel-position (frame x y)
  "Move the mouse pointer to pixel position (X,Y) in FRAME.\nThe position is given in pixels, where (0, 0) is the upper-left corner\nof the frame, X is the horizontal offset, and Y is the vertical offset."
  )

(defun frame-pixel-height (&optional frame)
  "Return a FRAME's height in pixels.\nIf FRAME is omitted, the selected frame is used.  The exact value\nof the result depends on the window-system and toolkit in use:"
  )

(defun frame-live-p (object)
  "Return non-nil if OBJECT is a frame which has not been deleted.\nValue is nil if OBJECT is not a live frame.  If object is a live\nframe, the return value indicates what sort of terminal device it is\ndisplayed on.  See the documentation of `framep' for possible\n"
  )

(defun set-frame-width (frame cols &optional pretend)
  "Specify that the frame FRAME has COLS columns.\nOptional third arg non-nil means that redisplay should use COLS columns\n"
  )

(defun next-frame (&optional frame miniframe)
  "Return the next frame in the frame list after FRAME.\nIt considers only frames on the same terminal as FRAME.\nBy default, skip minibuffer-only frames.\nIf omitted, FRAME defaults to the selected frame.\nIf optional argument MINIFRAME is nil, exclude minibuffer-only frames.\nIf MINIFRAME is a window, include only its own frame\nand any frame now using that window as the minibuffer.\nIf MINIFRAME is `visible', include all visible frames.\nIf MINIFRAME is 0, include all visible and iconified frames.\n"
  )

(defun frame-pixel-width (&optional frame)
  "Return FRAME's width in pixels.\nFor a terminal frame, the result really gives the width in characters.\n"
  )

(defun set-frame-height (frame lines &optional pretend)
  "Specify that the frame FRAME has LINES lines.\nOptional third arg non-nil means that redisplay should use LINES lines\nbut that the idea of the actual height of the frame should not be changed.move-to-column is an interactive built-in function in `C source code'."
  )

(defun frame-focus (frame)
  "Return the frame to which FRAME's keystrokes are currently being sent.\nThis returns nil if FRAME's focus is not redirected.\n"
  )

(defun frame-first-window (&optional frame)
  "Returns the topmost, leftmost window of FRAME.\n"
  )

(defun frame-root-window (&optional frame)
  "Returns the root-window of FRAME.\nIf omitted, FRAME defaults to the currently selected frame.downcase-word is an interactive built-in function in `C source code'."
  )

(defun set-frame-selected-window (frame window &optional norecord)
  "Set selected window of FRAME to WINDOW.\nIf FRAME is nil, use the selected frame.  If FRAME is the\nselected frame, this makes WINDOW the selected window.\nOptional argument NORECORD non-nil means to neither change the\norder of recently selected windows nor the buffer list.\n"
  )

(defun set-frame-position (frame xoffset yoffset)
  "Sets position of FRAME in pixels to XOFFSET by YOFFSET.\nThis is actually the position of the upper left corner of the frame.\nNegative values for XOFFSET or YOFFSET are interpreted relative to\n"
  )

(defun frame-selected-window (&optional frame)
  "Return the selected window of FRAME.\n"
  )

(defun frame-char-width (&optional frame)
  "Width in pixels of characters in the font in frame FRAME.\nIf FRAME is omitted, the selected frame is used.\nOn a graphical screen, the width is the standard width of the default font.\n"
  )

(defun mouse-position ()
  "Return a list (FRAME X . Y) giving the current mouse frame and position.\nThe position is given in character cells, where (0, 0) is the\nupper-left corner of the frame, X is the horizontal offset, and Y is\nthe vertical offset.\nIf Emacs is running on a mouseless terminal or hasn't been programmed\nto read the mouse position, it returns the selected frame for FRAME\nand nil for X and Y.\nIf `mouse-position-function' is non-nil, `mouse-position' calls it,\npassing the normal return value to that function as an argument,\n"
  )

(defun set-mouse-position (frame x y)
  "Move the mouse pointer to the center of character cell (X,Y) in FRAME.\nCoordinates are relative to the frame, not a window,\nso the coordinates of the top left character in the frame\nmay be nonzero due to left-hand scroll bars or the menu bar."
  )

(defun selected-frame ()
  )

(defun window-frame (window)
  )

(defun redirect-frame-focus (frame &optional focus-frame)
  "Arrange for keystrokes typed at FRAME to be sent to FOCUS-FRAME.\nIn other words, switch-frame events caused by events in FRAME will\nrequest a switch to FOCUS-FRAME, and `last-event-frame' will be\nFOCUS-FRAME after reading an event typed at FRAME."
  )

(defun visible-frame-list ()
  "Return a list of all frames now \"visible\" (being updated).re-search-backward is an interactive built-in function in `C source\ncode'."
  )

(defun frame-char-height (&optional frame)
  "Height in pixels of a line in the font in frame FRAME.\nIf FRAME is omitted, the selected frame is used.\n"
  )

(defun active-minibuffer-window ()
  "Return the currently active minibuffer window, or nil if none.forward-line is an interactive built-in function in `C source code'."
  )

(defun mouse-pixel-position ()
  "Return a list (FRAME X . Y) giving the current mouse frame and position.\nThe position is given in pixel units, where (0, 0) is the\nupper-left corner of the frame, X is the horizontal offset, and Y is\nthe vertical offset.\nIf Emacs is running on a mouseless terminal or hasn't been programmed\nto read the mouse position, it returns the selected frame for FRAME\n"
  )

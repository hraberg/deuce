(ns emacs.window (use [deuce.core]) (:refer-clojure :only []))

(defun window-live-p (object)
  )

(defun set-window-hscroll (window ncol)
  "Set number of columns WINDOW is scrolled from left margin to NCOL.\nReturn NCOL.  NCOL should be zero or positive."
  )

(defun window-configuration-p (object)
  )

(defun minibuffer-window (&optional frame)
  "Return the window used now for minibuffers.\nIf the optional argument FRAME is specified, return the minibuffer window\nused by that frame.backward-char is an interactive built-in function in `C source code'."
  )

(defun other-window-for-scrolling ()
  "Return the other window for \"other window scroll\" commands.\nIf `other-window-scroll-buffer' is non-nil, a window\nshowing that buffer is used.\nIf in the minibuffer, `minibuffer-scroll-window' if non-nil\nspecifies the window.  This takes precedence over\n`other-window-scroll-buffer'.word-search-forward-lax is an interactive built-in function in `C\nsource code'."
  )

(defun set-window-display-table (window table)
  "Set WINDOW's display-table to TABLE.base64-decode-region is an interactive built-in function in `C source\ncode'."
  )

(defun pos-visible-in-window-p (&optional pos window partially)
  "Return non-nil if position POS is currently on the frame in WINDOW.\nReturn nil if that position is scrolled vertically out of view.\nIf a character is only partially visible, nil is returned, unless the\noptional argument PARTIALLY is non-nil.\nIf POS is only out of view because of horizontal scrolling, return non-nil.\nIf POS is t, it specifies the position of the last visible glyph in WINDOW.\nPOS defaults to point in WINDOW; WINDOW defaults to the selected window."
  )

(defun window-end (&optional window update)
  "Return position at which display currently ends in WINDOW.\nWINDOW defaults to the selected window.\nThis is updated by redisplay, when it runs to completion.\nSimply changing the buffer text or setting `window-start'\ndoes not update this value.\nReturn nil if there is no recorded value.  (This can happen if the\nlast redisplay of WINDOW was preempted, and did not finish.)\nIf UPDATE is non-nil, compute the up-to-date position\n"
  )

(defun window-height (&optional window)
  "Return the number of lines in WINDOW.\nWINDOW defaults to the selected window."
  )

(defun set-window-margins (window left-width &optional right-width)
  "Set width of marginal areas of window WINDOW.\nIf WINDOW is nil, set margins of the currently selected window.\nSecond arg LEFT-WIDTH specifies the number of character cells to\nreserve for the left marginal area.  Optional third arg RIGHT-WIDTH\ndoes the same for the right marginal area.  A nil width parameter\n"
  )

(defun set-window-point (window pos)
  "Make point value in WINDOW be at position POS in WINDOW's buffer.\n"
  )

(defun window-point (&optional window)
  "Return current value of point in WINDOW.\nWINDOW defaults to the selected window."
  )

(defun window-pixel-edges (&optional window)
  "Return a list of the edge pixel coordinates of WINDOW.\nThe list has the form (LEFT TOP RIGHT BOTTOM), all relative to 0, 0 at\nthe top left corner of the frame."
  )

(defun get-lru-window (&optional frame dedicated)
  "Return the window least recently selected or used for display.\n(LRU means Least Recently Used.)"
  )

(defun set-window-parameter (window parameter value)
  "Set WINDOW's value of PARAMETER to VALUE.\n"
  )

(defun coordinates-in-window-p (coordinates window)
  "Return non-nil if COORDINATES are in WINDOW.\nCOORDINATES is a cons of the form (X . Y), X and Y being distances\nmeasured in characters from the upper-left corner of the frame.\n(0 . 0) denotes the character in the upper left corner of the\nframe.\nIf COORDINATES are in the text portion of WINDOW,\n   the coordinates relative to the window are returned.\nIf they are in the mode line of WINDOW, `mode-line' is returned.\nIf they are in the top mode line of WINDOW, `header-line' is returned.\nIf they are in the left fringe of WINDOW, `left-fringe' is returned.\nIf they are in the right fringe of WINDOW, `right-fringe' is returned.\nIf they are on the border between WINDOW and its right sibling,\n  `vertical-line' is returned.\nIf they are in the windows's left or right marginal areas, `left-margin'\n"
  )

(defun window-line-height (&optional line window)
  "Return height in pixels of text line LINE in window WINDOW.\nIf WINDOW is nil or omitted, use selected window."
  )

(defun set-window-scroll-bars (window width &optional vertical-type horizontal-type)
  "Set width and type of scroll bars of window WINDOW.\nIf window is nil, set scroll bars of the currently selected window.\nSecond parameter WIDTH specifies the pixel width for the scroll bar;\nthis is automatically adjusted to a multiple of the frame column width.\nThird parameter VERTICAL-TYPE specifies the type of the vertical scroll\nbar: left, right, or nil.\nIf WIDTH is nil, use the frame's scroll-bar width.\nIf VERTICAL-TYPE is t, use the frame's scroll-bar type.\n"
  )

(defun window-width (&optional window)
  "Return the number of display columns in WINDOW.\nWINDOW defaults to the selected window."
  )

(defun window-margins (&optional window)
  "Get width of marginal areas of window WINDOW.\nIf WINDOW is omitted or nil, use the currently selected window.\nValue is a cons of the form (LEFT-WIDTH . RIGHT-WIDTH).\nIf a marginal area does not exist, its width will be returned\n"
  )

(defun window-full-width-p (&optional window)
  "Return t if WINDOW is as wide as its frame.\n"
  )

(defun adjust-window-trailing-edge (window delta horizontal)
  "Adjust the bottom or right edge of WINDOW by DELTA.\nIf HORIZONTAL is non-nil, that means adjust the width, moving the right edge.\nOtherwise, adjust the height, moving the bottom edge."
  )

(defun window-inside-pixel-edges (&optional window)
  "Return a list of the edge pixel coordinates of WINDOW.\nThe list has the form (LEFT TOP RIGHT BOTTOM), all relative to 0, 0 at\nthe top left corner of the frame."
  )

(defun window-configuration-frame (config)
  )

(defun window-start (&optional window)
  "Return position at which display currently starts in WINDOW.\nWINDOW defaults to the selected window.\n"
  )

(defun set-window-buffer (window buffer-or-name &optional keep-margins)
  "Make WINDOW display BUFFER-OR-NAME as its contents.\nWINDOW defaults to the selected window.  BUFFER-OR-NAME must be a buffer\nor the name of an existing buffer.  Optional third argument KEEP-MARGINS\nnon-nil means that WINDOW's current display margins, fringe widths, and\nscroll bar settings are preserved; the default is to reset these from\nthe local settings for BUFFER-OR-NAME or the frame defaults.  Return nil."
  )

(defun window-parameter (window parameter)
  "Return WINDOW's value for PARAMETER.\n"
  )

(defun select-window (window &optional norecord)
  "Select WINDOW.  Most editing will apply to WINDOW's buffer.\nIf WINDOW is not already selected, make WINDOW's buffer current\nand make WINDOW the frame's selected window.  Return WINDOW.\nOptional second arg NORECORD non-nil means do not put this buffer\nat the front of the list of recently selected ones and do not\nmake this window the most recently selected one."
  )

(defun next-window (&optional window minibuf all-frames)
  "Return window following WINDOW in cyclic ordering of windows.\nWINDOW defaults to the selected window. The optional arguments\nMINIBUF and ALL-FRAMES specify the set of windows to consider."
  )

(defun window-minibuffer-p (&optional window)
  "Return non-nil if WINDOW is a minibuffer window.\n"
  )

(defun window-scroll-bars (&optional window)
  "Get width and type of scroll bars of window WINDOW.\nIf WINDOW is omitted or nil, use the currently selected window.\nValue is a list of the form (WIDTH COLS VERTICAL-TYPE HORIZONTAL-TYPE).\nIf WIDTH is nil or TYPE is t, the window is using the frame's corresponding\n"
  )

(defun window-dedicated-p (&optional window)
  "Return non-nil when WINDOW is dedicated to its buffer.\nMore precisely, return the value assigned by the last call of\n`set-window-dedicated-p' for WINDOW.  Return nil if that function was\nnever called with WINDOW as its argument, or the value set by that\nfunction was internally reset since its last call.  WINDOW defaults to\nthe selected window."
  )

(defun selected-window ()
  )

(defun set-window-vscroll (window vscroll &optional pixels-p)
  "Set amount by which WINDOW should be scrolled vertically to VSCROLL.\nWINDOW nil means use the selected window.  Normally, VSCROLL is a\nnon-negative multiple of the canonical character height of WINDOW;\noptional third arg PIXELS-P non-nil means that VSCROLL is in pixels.\nIf PIXELS-P is nil, VSCROLL may have to be rounded so that it\ncorresponds to an integral number of pixels.  The return value is the\nresult of this rounding.\n"
  )

(defun window-vscroll (&optional window pixels-p)
  "Return the amount by which WINDOW is scrolled vertically.\nUse the selected window if WINDOW is nil or omitted.\nNormally, value is a multiple of the canonical character height of WINDOW;\n"
  )

(defun get-buffer-window (&optional buffer-or-name frame)
  "Return a window currently displaying BUFFER-OR-NAME, or nil if none.\nBUFFER-OR-NAME may be a buffer or a buffer name and defaults to the\ncurrent buffer.\nIf optional argument FRAME is `visible', search all visible frames.\nIf optional argument FRAME is 0, search all visible and iconified frames.\nIf FRAME is t, search all frames.\nIf FRAME is nil, search only the selected frame.\n"
  )

(defun window-display-table (&optional window)
  "Return the display-table that WINDOW is using.\n"
  )

(defun window-tree (&optional frame)
  "Return the window tree for frame FRAME."
  )

(defun window-list (&optional frame minibuf window)
  "Return a list of windows on FRAME, starting with WINDOW.\nFRAME nil or omitted means use the selected frame.\nWINDOW nil or omitted means use the selected window.\nMINIBUF t means include the minibuffer window, even if it isn't active.\nMINIBUF nil or omitted means include the minibuffer window only\nif it's active.\n"
  )

(defun window-fringes (&optional window)
  "Get width of fringes of window WINDOW.\nIf WINDOW is omitted or nil, use the currently selected window.\nValue is a list of the form (LEFT-WIDTH RIGHT-WIDTH OUTSIDE-MARGINS).tty-suppress-bold-inverse-default-colors is a built-in function in `C\nsource code'."
  )

(defun window-parameters (&optional window)
  "Return the parameters of WINDOW and their values.\nWINDOW defaults to the selected window.  The return value is a list of\n"
  )

(defun window-at (x y &optional frame)
  "Return window containing coordinates X and Y on FRAME.\nIf omitted, FRAME defaults to the currently selected frame.\nThe top left corner of the frame is considered to be row 0,\ncolumn 0.make-frame-visible is an interactive built-in function in `C source\ncode'."
  )

(defun window-buffer (&optional window)
  "Return the buffer that WINDOW is displaying.\n"
  )

(defun set-window-configuration (configuration)
  "Set the configuration of windows and buffers as specified by CONFIGURATION.\nCONFIGURATION must be a value previously returned\nby `current-window-configuration' (which see).\nIf CONFIGURATION was made from a frame that is now deleted,\nonly frame-independent values can be restored.  In this case,\n"
  )

(defun force-window-update (&optional object)
  "Force all windows to be updated on next redisplay.\nIf optional arg OBJECT is a window, force redisplay of that window only.\nIf OBJECT is a buffer or buffer name, force redisplay of all windows\n"
  )

(defun set-window-fringes (window left-width &optional right-width outside-margins)
  "Set the fringe widths of window WINDOW.\nIf WINDOW is nil, set the fringe widths of the currently selected\nwindow.\nSecond arg LEFT-WIDTH specifies the number of pixels to reserve for\nthe left fringe.  Optional third arg RIGHT-WIDTH specifies the right\nfringe width.  If a fringe width arg is nil, that means to use the\nframe's default fringe width.  Default fringe widths can be set with\nthe command `set-fringe-style'.\nIf optional fourth arg OUTSIDE-MARGINS is non-nil, draw the fringes\noutside of the display margins.  By default, fringes are drawn between\n"
  )

(defun window-hscroll (&optional window)
  "Return the number of columns by which WINDOW is scrolled from left margin.\n"
  )

(defun get-largest-window (&optional frame dedicated)
  "Return the largest window in area.\nA minibuffer window is never a candidate.\nA dedicated window is never a candidate unless DEDICATED is non-nil,\n  so if all windows are dedicated, the value is nil.\nIf optional argument FRAME is `visible', search all visible frames.\nIf FRAME is 0, search all visible and iconified frames.\nIf FRAME is t, search all frames.\nIf FRAME is nil, search only the selected frame.\n"
  )

(defun minibuffer-selected-window ()
  "Return the window which was selected when entering the minibuffer.\n"
  )

(defun windowp (object)
  )

(defun set-window-dedicated-p (window flag)
  "Mark WINDOW as dedicated according to FLAG.\nWINDOW defaults to the selected window.  FLAG non-nil means mark WINDOW\nas dedicated to its buffer.  FLAG nil means mark WINDOW as non-dedicated.\nReturn FLAG."
  )

(defun window-text-height (&optional window)
  "Return the height in lines of the text display area of WINDOW.\nWINDOW defaults to the selected window."
  )

(defun window-inside-edges (&optional window)
  "Return a list of the edge coordinates of WINDOW.\nThe list has the form (LEFT TOP RIGHT BOTTOM).\nTOP and BOTTOM count by lines, and LEFT and RIGHT count by columns,\nall relative to 0, 0 at top left corner of frame."
  )

(defun previous-window (&optional window minibuf all-frames)
  "Return window preceding WINDOW in cyclic ordering of windows.\nWINDOW defaults to the selected window. The optional arguments\nMINIBUF and ALL-FRAMES specify the set of windows to consider.\nFor the precise meaning of these arguments see `next-window'."
  )

(defun set-window-start (window pos &optional noforce)
  "Make display in WINDOW start at position POS in WINDOW's buffer.\nWINDOW defaults to the selected window.  Return POS.\nOptional third arg NOFORCE non-nil inhibits next redisplay from\noverriding motion of point in order to display at this exact start.switch-to-buffer is an interactive built-in function in `C source\ncode'."
  )

(defun window-edges (&optional window)
  "Return a list of the edge coordinates of WINDOW.\nThe list has the form (LEFT TOP RIGHT BOTTOM).\nTOP and BOTTOM count by lines, and LEFT and RIGHT count by columns,\nall relative to 0, 0 at top left corner of frame."
  )

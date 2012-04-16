(ns emacs.window (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun window-live-p (object)
  )

(defun set-window-hscroll (window ncol)
  "Set number of columns WINDOW is scrolled from left margin to NCOL.
  Return NCOL.  NCOL should be zero or positive."
  )

(defun window-configuration-p (object)
  )

(defun minibuffer-window (&optional frame)
  "Return the window used now for minibuffers.
  If the optional argument FRAME is specified, return the minibuffer window
  used by that frame.backward-char is an interactive built-in function in `C source code'."
  )

(defun other-window-for-scrolling ()
  "Return the other window for \"other window scroll\" commands.
  If `other-window-scroll-buffer' is non-nil, a window
  showing that buffer is used.
  If in the minibuffer, `minibuffer-scroll-window' if non-nil
  specifies the window.  This takes precedence over
  `other-window-scroll-buffer'.word-search-forward-lax is an interactive built-in function in `C
  source code'."
  )

(defun set-window-display-table (window table)
  "Set WINDOW's display-table to TABLE.base64-decode-region is an interactive built-in function in `C source
  code'."
  )

(defun pos-visible-in-window-p (&optional pos window partially)
  "Return non-nil if position POS is currently on the frame in WINDOW.
  Return nil if that position is scrolled vertically out of view.
  If a character is only partially visible, nil is returned, unless the
  optional argument PARTIALLY is non-nil.
  If POS is only out of view because of horizontal scrolling, return non-nil.
  If POS is t, it specifies the position of the last visible glyph in WINDOW.
  POS defaults to point in WINDOW; WINDOW defaults to the selected window."
  )

(defun window-end (&optional window update)
  "Return position at which display currently ends in WINDOW.
  WINDOW defaults to the selected window.
  This is updated by redisplay, when it runs to completion.
  Simply changing the buffer text or setting `window-start'
  does not update this value.
  Return nil if there is no recorded value.  (This can happen if the
  last redisplay of WINDOW was preempted, and did not finish.)
  If UPDATE is non-nil, compute the up-to-date position"
  )

(defun window-height (&optional window)
  "Return the number of lines in WINDOW.
  WINDOW defaults to the selected window."
  )

(defun set-window-margins (window left-width &optional right-width)
  "Set width of marginal areas of window WINDOW.
  If WINDOW is nil, set margins of the currently selected window.
  Second arg LEFT-WIDTH specifies the number of character cells to
  reserve for the left marginal area.  Optional third arg RIGHT-WIDTH
  does the same for the right marginal area.  A nil width parameter"
  )

(defun set-window-point (window pos)
  "Make point value in WINDOW be at position POS in WINDOW's buffer."
  )

(defun window-point (&optional window)
  "Return current value of point in WINDOW.
  WINDOW defaults to the selected window."
  )

(defun window-pixel-edges (&optional window)
  "Return a list of the edge pixel coordinates of WINDOW.
  The list has the form (LEFT TOP RIGHT BOTTOM), all relative to 0, 0 at
  the top left corner of the frame."
  )

(defun get-lru-window (&optional frame dedicated)
  "Return the window least recently selected or used for display.
  (LRU means Least Recently Used.)"
  )

(defun set-window-parameter (window parameter value)
  "Set WINDOW's value of PARAMETER to VALUE."
  )

(defun coordinates-in-window-p (coordinates window)
  "Return non-nil if COORDINATES are in WINDOW.
  COORDINATES is a cons of the form (X . Y), X and Y being distances
  measured in characters from the upper-left corner of the frame.
  (0 . 0) denotes the character in the upper left corner of the
  frame.
  If COORDINATES are in the text portion of WINDOW,
     the coordinates relative to the window are returned.
  If they are in the mode line of WINDOW, `mode-line' is returned.
  If they are in the top mode line of WINDOW, `header-line' is returned.
  If they are in the left fringe of WINDOW, `left-fringe' is returned.
  If they are in the right fringe of WINDOW, `right-fringe' is returned.
  If they are on the border between WINDOW and its right sibling,
    `vertical-line' is returned.
  If they are in the windows's left or right marginal areas, `left-margin'"
  )

(defun window-line-height (&optional line window)
  "Return height in pixels of text line LINE in window WINDOW.
  If WINDOW is nil or omitted, use selected window."
  )

(defun set-window-scroll-bars (window width &optional vertical-type horizontal-type)
  "Set width and type of scroll bars of window WINDOW.
  If window is nil, set scroll bars of the currently selected window.
  Second parameter WIDTH specifies the pixel width for the scroll bar;
  this is automatically adjusted to a multiple of the frame column width.
  Third parameter VERTICAL-TYPE specifies the type of the vertical scroll
  bar: left, right, or nil.
  If WIDTH is nil, use the frame's scroll-bar width.
  If VERTICAL-TYPE is t, use the frame's scroll-bar type."
  )

(defun window-width (&optional window)
  "Return the number of display columns in WINDOW.
  WINDOW defaults to the selected window."
  )

(defun window-margins (&optional window)
  "Get width of marginal areas of window WINDOW.
  If WINDOW is omitted or nil, use the currently selected window.
  Value is a cons of the form (LEFT-WIDTH . RIGHT-WIDTH).
  If a marginal area does not exist, its width will be returned"
  )

(defun window-full-width-p (&optional window)
  "Return t if WINDOW is as wide as its frame."
  )

(defun adjust-window-trailing-edge (window delta horizontal)
  "Adjust the bottom or right edge of WINDOW by DELTA.
  If HORIZONTAL is non-nil, that means adjust the width, moving the right edge.
  Otherwise, adjust the height, moving the bottom edge."
  )

(defun window-inside-pixel-edges (&optional window)
  "Return a list of the edge pixel coordinates of WINDOW.
  The list has the form (LEFT TOP RIGHT BOTTOM), all relative to 0, 0 at
  the top left corner of the frame."
  )

(defun window-configuration-frame (config)
  )

(defun window-start (&optional window)
  "Return position at which display currently starts in WINDOW.
  WINDOW defaults to the selected window."
  )

(defun set-window-buffer (window buffer-or-name &optional keep-margins)
  "Make WINDOW display BUFFER-OR-NAME as its contents.
  WINDOW defaults to the selected window.  BUFFER-OR-NAME must be a buffer
  or the name of an existing buffer.  Optional third argument KEEP-MARGINS
  non-nil means that WINDOW's current display margins, fringe widths, and
  scroll bar settings are preserved; the default is to reset these from
  the local settings for BUFFER-OR-NAME or the frame defaults.  Return nil."
  )

(defun window-parameter (window parameter)
  "Return WINDOW's value for PARAMETER."
  )

(defun select-window (window &optional norecord)
  "Select WINDOW.  Most editing will apply to WINDOW's buffer.
  If WINDOW is not already selected, make WINDOW's buffer current
  and make WINDOW the frame's selected window.  Return WINDOW.
  Optional second arg NORECORD non-nil means do not put this buffer
  at the front of the list of recently selected ones and do not
  make this window the most recently selected one."
  )

(defun next-window (&optional window minibuf all-frames)
  "Return window following WINDOW in cyclic ordering of windows.
  WINDOW defaults to the selected window. The optional arguments
  MINIBUF and ALL-FRAMES specify the set of windows to consider."
  )

(defun window-minibuffer-p (&optional window)
  "Return non-nil if WINDOW is a minibuffer window."
  )

(defun window-scroll-bars (&optional window)
  "Get width and type of scroll bars of window WINDOW.
  If WINDOW is omitted or nil, use the currently selected window.
  Value is a list of the form (WIDTH COLS VERTICAL-TYPE HORIZONTAL-TYPE).
  If WIDTH is nil or TYPE is t, the window is using the frame's corresponding"
  )

(defun window-dedicated-p (&optional window)
  "Return non-nil when WINDOW is dedicated to its buffer.
  More precisely, return the value assigned by the last call of
  `set-window-dedicated-p' for WINDOW.  Return nil if that function was
  never called with WINDOW as its argument, or the value set by that
  function was internally reset since its last call.  WINDOW defaults to
  the selected window."
  )

(defun selected-window ()
  )

(defun set-window-vscroll (window vscroll &optional pixels-p)
  "Set amount by which WINDOW should be scrolled vertically to VSCROLL.
  WINDOW nil means use the selected window.  Normally, VSCROLL is a
  non-negative multiple of the canonical character height of WINDOW;
  optional third arg PIXELS-P non-nil means that VSCROLL is in pixels.
  If PIXELS-P is nil, VSCROLL may have to be rounded so that it
  corresponds to an integral number of pixels.  The return value is the
  result of this rounding."
  )

(defun window-vscroll (&optional window pixels-p)
  "Return the amount by which WINDOW is scrolled vertically.
  Use the selected window if WINDOW is nil or omitted.
  Normally, value is a multiple of the canonical character height of WINDOW;"
  )

(defun get-buffer-window (&optional buffer-or-name frame)
  "Return a window currently displaying BUFFER-OR-NAME, or nil if none.
  BUFFER-OR-NAME may be a buffer or a buffer name and defaults to the
  current buffer.
  If optional argument FRAME is `visible', search all visible frames.
  If optional argument FRAME is 0, search all visible and iconified frames.
  If FRAME is t, search all frames.
  If FRAME is nil, search only the selected frame."
  )

(defun window-display-table (&optional window)
  "Return the display-table that WINDOW is using."
  )

(defun window-tree (&optional frame)
  "Return the window tree for frame FRAME."
  )

(defun window-list (&optional frame minibuf window)
  "Return a list of windows on FRAME, starting with WINDOW.
  FRAME nil or omitted means use the selected frame.
  WINDOW nil or omitted means use the selected window.
  MINIBUF t means include the minibuffer window, even if it isn't active.
  MINIBUF nil or omitted means include the minibuffer window only
  if it's active."
  )

(defun window-fringes (&optional window)
  "Get width of fringes of window WINDOW.
  If WINDOW is omitted or nil, use the currently selected window.
  Value is a list of the form (LEFT-WIDTH RIGHT-WIDTH OUTSIDE-MARGINS).tty-suppress-bold-inverse-default-colors is a built-in function in `C
  source code'."
  )

(defun window-parameters (&optional window)
  "Return the parameters of WINDOW and their values.
  WINDOW defaults to the selected window.  The return value is a list of"
  )

(defun window-at (x y &optional frame)
  "Return window containing coordinates X and Y on FRAME.
  If omitted, FRAME defaults to the currently selected frame.
  The top left corner of the frame is considered to be row 0,
  column 0.make-frame-visible is an interactive built-in function in `C source
  code'."
  )

(defun window-buffer (&optional window)
  "Return the buffer that WINDOW is displaying."
  )

(defun set-window-configuration (configuration)
  "Set the configuration of windows and buffers as specified by CONFIGURATION.
  CONFIGURATION must be a value previously returned
  by `current-window-configuration' (which see).
  If CONFIGURATION was made from a frame that is now deleted,
  only frame-independent values can be restored.  In this case,"
  )

(defun force-window-update (&optional object)
  "Force all windows to be updated on next redisplay.
  If optional arg OBJECT is a window, force redisplay of that window only.
  If OBJECT is a buffer or buffer name, force redisplay of all windows"
  )

(defun set-window-fringes (window left-width &optional right-width outside-margins)
  "Set the fringe widths of window WINDOW.
  If WINDOW is nil, set the fringe widths of the currently selected
  window.
  Second arg LEFT-WIDTH specifies the number of pixels to reserve for
  the left fringe.  Optional third arg RIGHT-WIDTH specifies the right
  fringe width.  If a fringe width arg is nil, that means to use the
  frame's default fringe width.  Default fringe widths can be set with
  the command `set-fringe-style'.
  If optional fourth arg OUTSIDE-MARGINS is non-nil, draw the fringes
  outside of the display margins.  By default, fringes are drawn between"
  )

(defun window-hscroll (&optional window)
  "Return the number of columns by which WINDOW is scrolled from left margin."
  )

(defun get-largest-window (&optional frame dedicated)
  "Return the largest window in area.
  A minibuffer window is never a candidate.
  A dedicated window is never a candidate unless DEDICATED is non-nil,
    so if all windows are dedicated, the value is nil.
  If optional argument FRAME is `visible', search all visible frames.
  If FRAME is 0, search all visible and iconified frames.
  If FRAME is t, search all frames.
  If FRAME is nil, search only the selected frame."
  )

(defun minibuffer-selected-window ()
  "Return the window which was selected when entering the minibuffer."
  )

(defun windowp (object)
  )

(defun set-window-dedicated-p (window flag)
  "Mark WINDOW as dedicated according to FLAG.
  WINDOW defaults to the selected window.  FLAG non-nil means mark WINDOW
  as dedicated to its buffer.  FLAG nil means mark WINDOW as non-dedicated.
  Return FLAG."
  )

(defun window-text-height (&optional window)
  "Return the height in lines of the text display area of WINDOW.
  WINDOW defaults to the selected window."
  )

(defun window-inside-edges (&optional window)
  "Return a list of the edge coordinates of WINDOW.
  The list has the form (LEFT TOP RIGHT BOTTOM).
  TOP and BOTTOM count by lines, and LEFT and RIGHT count by columns,
  all relative to 0, 0 at top left corner of frame."
  )

(defun previous-window (&optional window minibuf all-frames)
  "Return window preceding WINDOW in cyclic ordering of windows.
  WINDOW defaults to the selected window. The optional arguments
  MINIBUF and ALL-FRAMES specify the set of windows to consider.
  For the precise meaning of these arguments see `next-window'."
  )

(defun set-window-start (window pos &optional noforce)
  "Make display in WINDOW start at position POS in WINDOW's buffer.
  WINDOW defaults to the selected window.  Return POS.
  Optional third arg NOFORCE non-nil inhibits next redisplay from
  overriding motion of point in order to display at this exact start.switch-to-buffer is an interactive built-in function in `C source
  code'."
  )

(defun window-edges (&optional window)
  "Return a list of the edge coordinates of WINDOW.
  The list has the form (LEFT TOP RIGHT BOTTOM).
  TOP and BOTTOM count by lines, and LEFT and RIGHT count by columns,
  all relative to 0, 0 at top left corner of frame."
  )

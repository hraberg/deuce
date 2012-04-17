(ns
 emacs.window
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun delete-windows-on (&optional buffer-or-name frame)
  "Delete all windows showing BUFFER-OR-NAME.
  BUFFER-OR-NAME may be a buffer or the name of an existing buffer and
  defaults to the current buffer."
  )

(defun window-live-p (object)
  "Return t if OBJECT is a window which is currently visible."
  )

(defun other-window (count &optional all-frames)
  "Select another window in cyclic ordering of windows.
  COUNT specifies the number of windows to skip, starting with the
  selected window, before making the selection.  If COUNT is
  positive, skip COUNT windows forwards.  If COUNT is negative,
  skip -COUNT windows backwards.  COUNT zero means do not skip any
  window, so select the selected window.  In an interactive call,
  COUNT is the numeric prefix argument.  Return nil."
  )

(defun scroll-other-window (&optional arg)
  "Scroll next window upward ARG lines; or near full screen if no ARG.
  A near full screen is `next-screen-context-lines' less than a full screen.
  The next window is the one below the current one; or the one at the top
  if the current one is at the bottom.  Negative ARG means scroll downward.
  If ARG is the atom `-', scroll downward by nearly full screen.
  When calling from a program, supply as argument a number, nil, or `-'."
  )

(defun set-window-hscroll (window ncol)
  "Set number of columns WINDOW is scrolled from left margin to NCOL.
  Return NCOL.  NCOL should be zero or positive."
  )

(defun recenter (&optional arg)
  "Center point in selected window and maybe redisplay frame.
  With prefix argument ARG, recenter putting point on screen line ARG
  relative to the selected window.  If ARG is negative, it counts up from the
  bottom of the window.  (ARG should be less than the height of the window.)"
  )

(defun scroll-down (&optional arg)
  "Scroll text of selected window down ARG lines.
  If ARG is omitted or nil, scroll down by a near full screen.
  A near full screen is `next-screen-context-lines' less than a full screen.
  Negative ARG means scroll upward.
  If ARG is the atom `-', scroll upward by nearly full screen.
  When calling from a program, supply as argument a number, nil, or `-'."
  )

(defun window-configuration-p (object)
  "Return t if OBJECT is a window-configuration object."
  )

(defun minibuffer-window (&optional frame)
  "Return the window used now for minibuffers.
  If the optional argument FRAME is specified, return the minibuffer window
  used by that frame."
  )

(defun scroll-up (&optional arg)
  "Scroll text of selected window upward ARG lines.
  If ARG is omitted or nil, scroll upward by a near full screen.
  A near full screen is `next-screen-context-lines' less than a full screen.
  Negative ARG means scroll downward.
  If ARG is the atom `-', scroll downward by nearly full screen.
  When calling from a program, supply as argument a number, nil, or `-'."
  )

(defun other-window-for-scrolling ()
  "Return the other window for \"other window scroll\" commands.
  If `other-window-scroll-buffer' is non-nil, a window
  showing that buffer is used.
  If in the minibuffer, `minibuffer-scroll-window' if non-nil
  specifies the window.  This takes precedence over
  `other-window-scroll-buffer'."
  )

(defun set-window-display-table (window table)
  "Set WINDOW's display-table to TABLE."
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
  If UPDATE is non-nil, compute the up-to-date position
  if it isn't already recorded."
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
  does the same for the right marginal area.  A nil width parameter
  means no margin."
  )

(defun set-window-point (window pos)
  "Make point value in WINDOW be at position POS in WINDOW's buffer.
  Return POS."
  )

(defun window-point (&optional window)
  "Return current value of point in WINDOW.
  WINDOW defaults to the selected window."
  )

(defun delete-other-windows (&optional window)
  "Make WINDOW (or the selected window) fill its frame.
  Only the frame WINDOW is on is affected.
  This function tries to reduce display jumps by keeping the text
  previously visible in WINDOW in the same place on the frame.  Doing this
  depends on the value of (window-start WINDOW), so if calling this
  function in a program gives strange scrolling, make sure the
  window-start value is reasonable when this function is called."
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

(defun set-window-redisplay-end-trigger (window value)
  "This function is obsolete since 23.1."
  )

(defun current-window-configuration (&optional frame)
  "Return an object representing the current window configuration of FRAME.
  If FRAME is nil or omitted, use the selected frame.
  This describes the number of windows, their sizes and current buffers,
  and for each displayed buffer, where display starts, and the positions of
  point and mark.  An exception is made for point in the current buffer:
  its value is -not- saved.
  This also records the currently selected frame, and FRAME's focus
  redirection (see `redirect-frame-focus')."
  )

(defun set-window-parameter (window parameter value)
  "Set WINDOW's value of PARAMETER to VALUE.
  WINDOW defaults to the selected window.  Return VALUE."
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
  If they are in the windows's left or right marginal areas, `left-margin'
    or `right-margin' is returned."
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
  If VERTICAL-TYPE is t, use the frame's scroll-bar type.
  Fourth parameter HORIZONTAL-TYPE is currently unused."
  )

(defun window-width (&optional window)
  "Return the number of display columns in WINDOW.
  WINDOW defaults to the selected window."
  )

(defun window-margins (&optional window)
  "Get width of marginal areas of window WINDOW.
  If WINDOW is omitted or nil, use the currently selected window.
  Value is a cons of the form (LEFT-WIDTH . RIGHT-WIDTH).
  If a marginal area does not exist, its width will be returned
  as nil."
  )

(defun window-full-width-p (&optional window)
  "Return t if WINDOW is as wide as its frame.
  WINDOW defaults to the selected window."
  )

(defun replace-buffer-in-windows (&optional buffer-or-name)
  "Replace BUFFER-OR-NAME with some other buffer in all windows showing it.
  BUFFER-OR-NAME may be a buffer or the name of an existing buffer and
  defaults to the current buffer."
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
  "Return the frame that CONFIG, a window-configuration object, is about."
  )

(defun window-start (&optional window)
  "Return position at which display currently starts in WINDOW.
  WINDOW defaults to the selected window.
  This is updated by redisplay or by calling `set-window-start'."
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
  "Return WINDOW's value for PARAMETER.
  WINDOW defaults to the selected window."
  )

(defun window-redisplay-end-trigger (&optional window)
  "This function is obsolete since 23.1."
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
  "Return non-nil if WINDOW is a minibuffer window.
  WINDOW defaults to the selected window."
  )

(defun window-scroll-bars (&optional window)
  "Get width and type of scroll bars of window WINDOW.
  If WINDOW is omitted or nil, use the currently selected window.
  Value is a list of the form (WIDTH COLS VERTICAL-TYPE HORIZONTAL-TYPE).
  If WIDTH is nil or TYPE is t, the window is using the frame's corresponding
  value."
  )

(defun shrink-window (size &optional horizontal)
  "Make selected window SIZE lines smaller.
  Interactively, if no argument is given, make the selected window one
  line smaller.  If optional argument HORIZONTAL is non-nil, make the
  window narrower by SIZE columns.  If SIZE is negative, enlarge selected
  window by -SIZE lines or columns.  Return nil."
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
  "Return the window that the cursor now appears in and commands apply to."
  )

(defun move-to-window-line (arg)
  "Position point relative to window.
  With no argument, position point at center of window.
  An argument specifies vertical position within the window;
  zero means top of window, negative means relative to bottom of window."
  )

(defun set-window-vscroll (window vscroll &optional pixels-p)
  "Set amount by which WINDOW should be scrolled vertically to VSCROLL.
  WINDOW nil means use the selected window.  Normally, VSCROLL is a
  non-negative multiple of the canonical character height of WINDOW;
  optional third arg PIXELS-P non-nil means that VSCROLL is in pixels.
  If PIXELS-P is nil, VSCROLL may have to be rounded so that it
  corresponds to an integral number of pixels.  The return value is the
  result of this rounding.
  If PIXELS-P is non-nil, the return value is VSCROLL."
  )

(defun window-vscroll (&optional window pixels-p)
  "Return the amount by which WINDOW is scrolled vertically.
  Use the selected window if WINDOW is nil or omitted.
  Normally, value is a multiple of the canonical character height of WINDOW;
  optional second arg PIXELS-P means value is measured in pixels."
  )

(defun get-buffer-window (&optional buffer-or-name frame)
  "Return a window currently displaying BUFFER-OR-NAME, or nil if none.
  BUFFER-OR-NAME may be a buffer or a buffer name and defaults to the
  current buffer.
  If optional argument FRAME is `visible', search all visible frames.
  If optional argument FRAME is 0, search all visible and iconified frames.
  If FRAME is t, search all frames.
  If FRAME is nil, search only the selected frame.
  If FRAME is a frame, search only that frame."
  )

(defun window-display-table (&optional window)
  "Return the display-table that WINDOW is using.
  WINDOW defaults to the selected window."
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
  if it's active.
  MINIBUF neither nil nor t means never include the minibuffer window."
  )

(defun window-fringes (&optional window)
  "Get width of fringes of window WINDOW.
  If WINDOW is omitted or nil, use the currently selected window.
  Value is a list of the form (LEFT-WIDTH RIGHT-WIDTH OUTSIDE-MARGINS)."
  )

(defun window-parameters (&optional window)
  "Return the parameters of WINDOW and their values.
  WINDOW defaults to the selected window.  The return value is a list of
  elements of the form (PARAMETER . VALUE)."
  )

(defun window-at (x y &optional frame)
  "Return window containing coordinates X and Y on FRAME.
  If omitted, FRAME defaults to the currently selected frame.
  The top left corner of the frame is considered to be row 0,
  column 0."
  )

(defun window-buffer (&optional window)
  "Return the buffer that WINDOW is displaying.
  WINDOW defaults to the selected window."
  )

(defun set-window-configuration (configuration)
  "Set the configuration of windows and buffers as specified by CONFIGURATION.
  CONFIGURATION must be a value previously returned
  by `current-window-configuration' (which see).
  If CONFIGURATION was made from a frame that is now deleted,
  only frame-independent values can be restored.  In this case,
  the return value is nil.  Otherwise the value is t."
  )

(defun force-window-update (&optional object)
  "Force all windows to be updated on next redisplay.
  If optional arg OBJECT is a window, force redisplay of that window only.
  If OBJECT is a buffer or buffer name, force redisplay of all windows
  displaying that buffer."
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
  outside of the display margins.  By default, fringes are drawn between
  display marginal areas and the text area."
  )

(defun window-hscroll (&optional window)
  "Return the number of columns by which WINDOW is scrolled from left margin.
  WINDOW defaults to the selected window."
  )

(defun get-largest-window (&optional frame dedicated)
  "Return the largest window in area.
  A minibuffer window is never a candidate.
  A dedicated window is never a candidate unless DEDICATED is non-nil,
    so if all windows are dedicated, the value is nil.
  If optional argument FRAME is `visible', search all visible frames.
  If FRAME is 0, search all visible and iconified frames.
  If FRAME is t, search all frames.
  If FRAME is nil, search only the selected frame.
  If FRAME is a frame, search only that frame."
  )

(defun minibuffer-selected-window ()
  "Return the window which was selected when entering the minibuffer.
  Returns nil, if selected window is not a minibuffer window."
  )

(defun enlarge-window (size &optional horizontal)
  "Make selected window SIZE lines taller.
  Interactively, if no argument is given, make the selected window one
  line taller.  If optional argument HORIZONTAL is non-nil, make selected
  window wider by SIZE columns.  If SIZE is negative, shrink the window by
  -SIZE lines or columns.  Return nil."
  )

(defun windowp (object)
  "Return t if OBJECT is a window."
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

(defun split-window (&optional window size horizontal)
  "Split WINDOW, putting SIZE lines in the first of the pair.
  WINDOW defaults to selected one and SIZE to half its size.
  If optional third arg HORIZONTAL is non-nil, split side by side and put
  SIZE columns in the first of the pair.  In that case, SIZE includes that
  window's scroll bar, or the divider column to its right.
  Interactively, all arguments are nil.
  Returns the newly created window (which is the lower or rightmost one).
  The upper or leftmost window is the original one, and remains selected
  if it was selected before."
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

(defun scroll-right (&optional arg set-minimum)
  "Scroll selected window display ARG columns right.
  Default for ARG is window width minus 2.
  Value is the total amount of leftward horizontal scrolling in
  effect after the change.
  If SET-MINIMUM is non-nil, the new scroll amount becomes the
  lower bound for automatic scrolling, i.e. automatic scrolling
  will not scroll a window to a column less than the value returned
  by this function.  This happens in an interactive call."
  )

(defun set-window-start (window pos &optional noforce)
  "Make display in WINDOW start at position POS in WINDOW's buffer.
  WINDOW defaults to the selected window.  Return POS.
  Optional third arg NOFORCE non-nil inhibits next redisplay from
  overriding motion of point in order to display at this exact start."
  )

(defun scroll-left (&optional arg set-minimum)
  "Scroll selected window display ARG columns left.
  Default for ARG is window width minus 2.
  Value is the total amount of leftward horizontal scrolling in
  effect after the change.
  If SET-MINIMUM is non-nil, the new scroll amount becomes the
  lower bound for automatic scrolling, i.e. automatic scrolling
  will not scroll a window to a column less than the value returned
  by this function.  This happens in an interactive call."
  )

(defun window-edges (&optional window)
  "Return a list of the edge coordinates of WINDOW.
  The list has the form (LEFT TOP RIGHT BOTTOM).
  TOP and BOTTOM count by lines, and LEFT and RIGHT count by columns,
  all relative to 0, 0 at top left corner of frame."
  )

(defun delete-window (&optional window)
  "Remove WINDOW from its frame.
  WINDOW defaults to the selected window.  Return nil.
  Signal an error when WINDOW is the only window on its frame."
  )

(defun compare-window-configurations (x y)
  "Compare two window configurations as regards the structure of windows.
  This function ignores details such as the values of point and mark
  and scrolling positions."
  )

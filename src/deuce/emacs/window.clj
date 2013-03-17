(ns deuce.emacs.window
  (:use [deuce.emacs-lisp :only (defun defvar) :as el])
  (:require [clojure.core :as c]
            [deuce.emacs.buffer :as buffer]
            [deuce.emacs-lisp.cons :as cons])
  (:import [deuce.emacs.data Window])
  (:refer-clojure :exclude []))

(defvar window-combination-limit nil
  "If t, splitting a window makes a new parent window.
  If this variable is nil, splitting a window will create a new parent
  window only if the window has no parent window or the window shall
  become a combination orthogonal to the one it is part of.

  If this variable is t, splitting a window always creates a new parent
  window.  If all splits behave this way, each frame's window tree is a
  binary tree and every window but the frame's root window has exactly one
  sibling.

  Other values are reserved for future use.

  The value of this variable is also assigned to the combination limit of
  the new parent window.  The combination limit of a window can be
  retrieved via the function `window-combination-limit' and altered by the
  function `set-window-combination-limit'.

  You can customize this variable.")

(defvar window-configuration-change-hook nil
  "Functions to call when window configuration changes.
  The buffer-local part is run once per window, with the relevant window
  selected; while the global part is run only once for the modified frame,
  with the relevant frame selected.")

(defvar other-window-scroll-buffer nil
  "If non-nil, this is a buffer and C-M-v should scroll its window.")

(defvar window-persistent-parameters nil
  "Alist of persistent window parameters.
  This alist specifies which window parameters shall get saved by
  `current-window-configuration' and `window-state-get' and subsequently
  restored to their previous values by `set-window-configuration' and
  `window-state-put'.

  The car of each entry of this alist is the symbol specifying the
  parameter.  The cdr is one of the following:

  nil means the parameter is neither saved by `window-state-get' nor by
  `current-window-configuration'.

  t means the parameter is saved by `current-window-configuration' and,
  provided its WRITABLE argument is nil, by `window-state-get'.

  The symbol `writable' means the parameter is saved unconditionally by
  both `current-window-configuration' and `window-state-get'.  Do not use
  this value for parameters without read syntax (like windows or frames).

  Parameters not saved by `current-window-configuration' or
  `window-state-get' are left alone by `set-window-configuration'
  respectively are not installed by `window-state-put'.")

(defvar minibuffer-scroll-window nil
  "Non-nil means it is the window that C-M-v in minibuffer should scroll.")

(defvar auto-window-vscroll nil
  "Non-nil means to automatically adjust `window-vscroll' to view tall lines.")

(defvar mode-line-in-non-selected-windows nil
  "Non-nil means to use `mode-line-inactive' face in non-selected windows.
  If the minibuffer is active, the `minibuffer-scroll-window' mode line
  is displayed in the `mode-line' face.

  You can customize this variable.")

(defvar temp-buffer-show-function nil
  "Non-nil means call as function to display a help buffer.
  The function is called with one argument, the buffer to be displayed.
  Used by `with-output-to-temp-buffer'.
  If this function is used, then it must do the entire job of showing
  the buffer; `temp-buffer-show-hook' is not run unless this function runs it.

  You can customize this variable.")

(defvar next-screen-context-lines nil
  "Number of lines of continuity when scrolling by screenfuls.

  You can customize this variable.")

(defvar window-point-insertion-type nil
  "Type of marker to use for `window-point'.")

(defvar recenter-redisplay nil
  "Non-nil means `recenter' redraws entire frame.
  If this option is non-nil, then the `recenter' command with a nil
  argument will redraw the entire frame; the special value `tty' causes
  the frame to be redrawn only if it is a tty frame.

  You can customize this variable.")

(defvar scroll-preserve-screen-position nil
  "Controls if scroll commands move point to keep its screen position unchanged.
  A value of nil means point does not keep its screen position except
  at the scroll margin or window boundary respectively.
  A value of t means point keeps its screen position if the scroll
  command moved it vertically out of the window, e.g. when scrolling
  by full screens.
  Any other value means point always keeps its screen position.
  Scroll commands should have the `scroll-command' property
  on their symbols to be controlled by this variable.

  You can customize this variable.")

(defvar window-combination-resize nil
  "If t, resize window combinations proportionally.
  If this variable is nil, splitting a window gets the entire screen space
  for displaying the new window from the window to split.  Deleting and
  resizing a window preferably resizes one adjacent window only.

  If this variable is t, splitting a window tries to get the space
  proportionally from all windows in the same combination.  This also
  allows to split a window that is otherwise too small or of fixed size.
  Resizing and deleting a window proportionally resize all windows in the
  same combination.

  Other values are reserved for future use.

  This variable takes no effect if `window-combination-limit' is non-nil.

  You can customize this variable.")

(declare windowp selected-window)

(defun window-live-p (object)
  "Return t if OBJECT is a live window and nil otherwise.
  A live window is a window that displays a buffer.
  Internal windows and deleted windows are not live."
  (windowp object))

(defun window-combination-limit (window)
  "Return combination limit of window WINDOW.
  If the return value is nil, child windows of WINDOW can be recombined with
  WINDOW's siblings.  A return value of t means that child windows of
  WINDOW are never (re-)combined with WINDOW's siblings."
  )

(defun window-total-width (&optional window)
  "Return the total width, in columns, of window WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window.

  The return value includes any vertical dividers or scroll bars
  belonging to WINDOW.  If WINDOW is an internal window, the total width
  is the width of the screen areas spanned by its children.

  On a graphical display, this total width is reported as an
  integer multiple of the default character width."
  )

(defun window-normal-size (&optional window horizontal)
  "Return the normal height of window WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window.
  If HORIZONTAL is non-nil, return the normal width of WINDOW."
  )

(defun scroll-other-window (&optional arg)
  "Scroll next window upward ARG lines; or near full screen if no ARG.
  A near full screen is `next-screen-context-lines' less than a full screen.
  The next window is the one below the current one; or the one at the top
  if the current one is at the bottom.  Negative ARG means scroll downward.
  If ARG is the atom `-', scroll downward by nearly full screen.
  When calling from a program, supply as argument a number, nil, or `-'.

  If `other-window-scroll-buffer' is non-nil, scroll the window
  showing that buffer, popping the buffer up if necessary.
  If in the minibuffer, `minibuffer-scroll-window' if non-nil
  specifies the window to scroll.  This takes precedence over
  `other-window-scroll-buffer'."
  )

(defun window-inside-absolute-pixel-edges (&optional window)
  "Return a list of the edge pixel coordinates of WINDOW's text area.
  The list has the form (LEFT TOP RIGHT BOTTOM), all relative to (0,0)
  at the top left corner of the frame's window area.

  RIGHT is one more than the rightmost x position of WINDOW's text area.
  BOTTOM is one more than the bottommost y position of WINDOW's text area.
  The inside edges do not include the space used by WINDOW's scroll bar,
  display margins, fringes, header line, and/or mode line."
  )

(defun set-window-hscroll (window ncol)
  "Set number of columns WINDOW is scrolled from left margin to NCOL.
  If WINDOW is nil, the selected window is used.
  Return NCOL.  NCOL should be zero or positive.

  Note that if `automatic-hscrolling' is non-nil, you cannot scroll the
  window so that the location of point moves off-window."
  )

(defun recenter (&optional arg)
  "Center point in selected window and maybe redisplay frame.
  With prefix argument ARG, recenter putting point on screen line ARG
  relative to the selected window.  If ARG is negative, it counts up from the
  bottom of the window.  (ARG should be less than the height of the window.)

  If ARG is omitted or nil, then recenter with point on the middle line of
  the selected window; if the variable `recenter-redisplay' is non-nil,
  also erase the entire frame and redraw it (when `auto-resize-tool-bars'
  is set to `grow-only', this resets the tool-bar's height to the minimum
  height needed); if `recenter-redisplay' has the special value `tty',
  then only tty frames are redrawn.

  Just C-u as prefix means put point in the center of the window
  and redisplay normally--don't erase and redraw the frame."
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
  "Return the minibuffer window for frame FRAME.
  If FRAME is omitted or nil, it defaults to the selected frame."
  (.minibuffer-window (or frame ((el/fun 'selected-frame)))))

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

(defun window-parent (&optional window)
  "Return the parent window of window WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window.
  Return nil for a window with no parent (e.g. a root window)."
  @(.parent (or window (selected-window))))

(defun pos-visible-in-window-p (&optional pos window partially)
  "Return non-nil if position POS is currently on the frame in WINDOW.
  Return nil if that position is scrolled vertically out of view.
  If a character is only partially visible, nil is returned, unless the
  optional argument PARTIALLY is non-nil.
  If POS is only out of view because of horizontal scrolling, return non-nil.
  If POS is t, it specifies the position of the last visible glyph in WINDOW.
  POS defaults to point in WINDOW; WINDOW defaults to the selected window.

  If POS is visible, return t if PARTIALLY is nil; if PARTIALLY is non-nil,
  return value is a list of 2 or 6 elements (X Y [RTOP RBOT ROWH VPOS]),
  where X and Y are the pixel coordinates relative to the top left corner
  of the window.  The remaining elements are omitted if the character after
  POS is fully visible; otherwise, RTOP and RBOT are the number of pixels
  off-window at the top and bottom of the row, ROWH is the height of the
  display row, and VPOS is the row number (0-based) containing POS."
  )

(defun resize-mini-window-internal (window)
  "Resize minibuffer window WINDOW."
  )

(defun window-end (&optional window update)
  "Return position at which display currently ends in WINDOW.
  WINDOW must be a live window and defaults to the selected one.
  This is updated by redisplay, when it runs to completion.
  Simply changing the buffer text or setting `window-start'
  does not update this value.
  Return nil if there is no recorded value.  (This can happen if the
  last redisplay of WINDOW was preempted, and did not finish.)
  If UPDATE is non-nil, compute the up-to-date position
  if it isn't already recorded."
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
  WINDOW must be a live window and defaults to the selected one.

  For a nonselected window, this is the value point would have
  if that window were selected.

  Note that, when WINDOW is the selected window and its buffer
  is also currently selected, the value returned is the same as (point).
  It would be more strictly correct to return the `top-level' value
  of point, outside of any save-excursion forms.
  But that is hard to define."
  )

(defun window-pixel-edges (&optional window)
  "Return a list of the edge pixel coordinates of WINDOW.
  The list has the form (LEFT TOP RIGHT BOTTOM), all relative to 0, 0 at
  the top left corner of the frame.

  RIGHT is one more than the rightmost x position occupied by WINDOW.
  BOTTOM is one more than the bottommost y position occupied by WINDOW.
  The pixel edges include the space used by WINDOW's scroll bar, display
  margins, fringes, header line, and/or mode line.  For the pixel edges
  of just the text area, use `window-inside-pixel-edges'."
  )

(defun window-left-column (&optional window)
  "Return left column of window WINDOW.
  This is the distance, in columns, between the left edge of WINDOW and
  the left edge of the frame's window area.  For instance, the return
  value is 0 if there is no window to the left of WINDOW.

  If WINDOW is omitted or nil, it defaults to the selected window."
  @(.left-col (or window (selected-window))))

(defun window-next-sibling (&optional window)
  "Return the next sibling window of window WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window.
  Return nil if WINDOW has no next sibling."
  )

(defun window-next-buffers (&optional window)
  "Return list of buffers recently re-shown in WINDOW.
  WINDOW must be a live window and defaults to the selected one."
  )

(defun set-window-combination-limit (window limit)
  "Set combination limit of window WINDOW to LIMIT; return LIMIT.
  If LIMIT is nil, child windows of WINDOW can be recombined with
  WINDOW's siblings.  LIMIT t means that child windows of WINDOW are
  never (re-)combined with WINDOW's siblings.  Other values are reserved
  for future use."
  )

(defun set-window-next-buffers (window next-buffers)
  "Set WINDOW's next buffers to NEXT-BUFFERS.
  WINDOW must be a live window and defaults to the selected one.
  NEXT-BUFFERS should be a list of buffers."
  )

(defun set-window-redisplay-end-trigger (window value)
  "This function is obsolete since 23.1.

  Set WINDOW's redisplay end trigger value to VALUE.
  VALUE should be a buffer position (typically a marker) or nil.
  If it is a buffer position, then if redisplay in WINDOW reaches a position
  beyond VALUE, the functions in `redisplay-end-trigger-functions' are called
  with two arguments: WINDOW, and the end trigger value.
  Afterwards the end-trigger value is reset to nil."
  )

(defun current-window-configuration (&optional frame)
  "Return an object representing the current window configuration of FRAME.
  If FRAME is nil or omitted, use the selected frame.
  This describes the number of windows, their sizes and current buffers,
  and for each displayed buffer, where display starts, and the positions of
  point and mark.  An exception is made for point in the current buffer:
  its value is -not- saved.
  This also records the currently selected frame, and FRAME's focus
  redirection (see `redirect-frame-focus').  The variable
  `window-persistent-parameters' specifies which window parameters are
  saved by this function."
  )

(defun set-window-parameter (window parameter value)
  "Set WINDOW's value of PARAMETER to VALUE.
  WINDOW defaults to the selected window.  Return VALUE."
  )

(defun coordinates-in-window-p (coordinates window)
  "Return non-nil if COORDINATES are in WINDOW.
  WINDOW must be a live window.
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

(defun window-top-line (&optional window)
  "Return top line of window WINDOW.
  This is the distance, in lines, between the top of WINDOW and the top
  of the frame's window area.  For instance, the return value is 0 if
  there is no window above WINDOW.

  If WINDOW is omitted or nil, it defaults to the selected window."
  @(.top-line (or window (selected-window))))

(defun window-line-height (&optional line window)
  "Return height in pixels of text line LINE in window WINDOW.
  WINDOW defaults to the selected window.

  Return height of current line if LINE is omitted or nil.  Return height of
  header or mode line if LINE is `header-line' or `mode-line'.
  Otherwise, LINE is a text line number starting from 0.  A negative number
  counts from the end of the window.

  Value is a list (HEIGHT VPOS YPOS OFFBOT), where HEIGHT is the height
  in pixels of the visible part of the line, VPOS and YPOS are the
  vertical position in lines and pixels of the line, relative to the top
  of the first text line, and OFFBOT is the number of off-window pixels at
  the bottom of the text line.  If there are off-window pixels at the top
  of the (first) text line, YPOS is negative.

  Return nil if window display is not up-to-date.  In that case, use
  `pos-visible-in-window-p' to obtain the information."
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

(defun run-window-configuration-change-hook (frame)
  "Run `window-configuration-change-hook' for FRAME."
  )

(defun window-margins (&optional window)
  "Get width of marginal areas of window WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window.
  Value is a cons of the form (LEFT-WIDTH . RIGHT-WIDTH).
  If a marginal area does not exist, its width will be returned
  as nil."
  )

(defun delete-other-windows-internal (&optional window root)
  "Make WINDOW fill its frame.
  Only the frame WINDOW is on is affected.  WINDOW may be any window and
  defaults to the selected one.

  Optional argument ROOT, if non-nil, must specify an internal window such
  that WINDOW is in its window subtree.  If this is the case, replace ROOT
  by WINDOW and leave alone any windows not part of ROOT's subtree.

  When WINDOW is live try to reduce display jumps by keeping the text
  previously visible in WINDOW in the same place on the frame.  Doing this
  depends on the value of (window-start WINDOW), so if calling this
  function in a program gives strange scrolling, make sure the
  window-start value is reasonable when this function is called."
  )

(defun window-inside-pixel-edges (&optional window)
  "Return a list of the edge pixel coordinates of WINDOW's text area.
  The list has the form (LEFT TOP RIGHT BOTTOM), all relative to (0,0)
  at the top left corner of the frame's window area.

  RIGHT is one more than the rightmost x position of WINDOW's text area.
  BOTTOM is one more than the bottommost y position of WINDOW's text area.
  The inside edges do not include the space used by WINDOW's scroll bar,
  display margins, fringes, header line, and/or mode line."
  )

(defun window-use-time (&optional window)
  "Return the use time of window WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window.
  The window with the highest use time is the most recently selected
  one.  The window with the lowest use time is the least recently
  selected one."
  )

(defun split-window-internal (old total-size side normal-size)
  "Split window OLD.
  Second argument TOTAL-SIZE specifies the number of lines or columns of the
  new window.  In any case TOTAL-SIZE must be a positive integer.

  Third argument SIDE nil (or `below') specifies that the new window shall
  be located below WINDOW.  SIDE `above' means the new window shall be
  located above WINDOW.  In both cases TOTAL-SIZE specifies the number of
  lines of the new window including space reserved for the mode and/or
  header line.

  SIDE t (or `right') specifies that the new window shall be located on
  the right side of WINDOW.  SIDE `left' means the new window shall be
  located on the left of WINDOW.  In both cases TOTAL-SIZE specifies the
  number of columns of the new window including space reserved for fringes
  and the scrollbar or a divider column.

  Fourth argument NORMAL-SIZE specifies the normal size of the new window
  according to the SIDE argument.

  The new total and normal sizes of all involved windows must have been
  set correctly.  See the code of `split-window' for how this is done."
  )

(defun window-configuration-frame (config)
  "Return the frame that CONFIG, a window-configuration object, is about."
  )

(defun set-window-new-total (window size &optional add)
  "Set new total size of WINDOW to SIZE.
  Return SIZE.

  Optional argument ADD non-nil means add SIZE to the new total size of
  WINDOW and return the sum.

  Note: This function does not operate on any child windows of WINDOW."
  )

(defun window-start (&optional window)
  "Return position at which display currently starts in WINDOW.
  WINDOW must be a live window and defaults to the selected one.
  This is updated by redisplay or by calling `set-window-start'."
  )

(defun set-window-buffer (window buffer-or-name &optional keep-margins)
  "Make WINDOW display BUFFER-OR-NAME as its contents.
  WINDOW has to be a live window and defaults to the selected one.
  BUFFER-OR-NAME must be a buffer or the name of an existing buffer.

  Optional third argument KEEP-MARGINS non-nil means that WINDOW's current
  display margins, fringe widths, and scroll bar settings are preserved;
  the default is to reset these from the local settings for BUFFER-OR-NAME
  or the frame defaults.  Return nil.

  This function throws an error when WINDOW is strongly dedicated to its
  buffer (that is `window-dedicated-p' returns t for WINDOW) and does not
  already display BUFFER-OR-NAME.

  This function runs `window-scroll-functions' before running
  `window-configuration-change-hook'."
  (reset! (.buffer (el/check-type 'windowp
                                  (or window (selected-window))))
          (el/check-type 'bufferp (buffer/get-buffer buffer-or-name)))
  nil)

(defun window-parameter (window parameter)
  "Return WINDOW's value for PARAMETER.
  WINDOW defaults to the selected window."
  )

(defun window-redisplay-end-trigger (&optional window)
  "This function is obsolete since 23.1.

  Return WINDOW's redisplay end trigger value.
  WINDOW defaults to the selected window.
  See `set-window-redisplay-end-trigger' for more information."
  )

(defun select-window (window &optional norecord)
  "Select WINDOW.  Most editing will apply to WINDOW's buffer.
  Also make WINDOW's buffer current and make WINDOW the frame's selected
  window.  Return WINDOW.

  Optional second arg NORECORD non-nil means do not put this buffer at the
  front of the buffer list and do not make this window the most recently
  selected one.

  Note that the main editor command loop sets the current buffer to the
  buffer of the selected window before each command."
  (el/check-type 'windowp window)
  (reset! (.selected-window ((el/fun 'selected-frame))) window))

(defun window-absolute-pixel-edges (&optional window)
  "Return a list of the edge pixel coordinates of WINDOW.
  The list has the form (LEFT TOP RIGHT BOTTOM), all relative to 0, 0 at
  the top left corner of the display.

  RIGHT is one more than the rightmost x position occupied by WINDOW.
  BOTTOM is one more than the bottommost y position occupied by WINDOW.
  The pixel edges include the space used by WINDOW's scroll bar, display
  margins, fringes, header line, and/or mode line.  For the pixel edges
  of just the text area, use `window-inside-absolute-pixel-edges'."
  )

(defun window-total-height (&optional window)
  "Return the total height, in lines, of window WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window.

  The return value includes the mode line and header line, if any.
  If WINDOW is an internal window, the total height is the height
  of the screen areas spanned by its children.

  On a graphical display, this total height is reported as an
  integer multiple of the default character height."
  )

(defun next-window (&optional window minibuf all-frames)
  "Return live window after WINDOW in the cyclic ordering of windows.
  WINDOW must be a live window and defaults to the selected one.  The
  optional arguments MINIBUF and ALL-FRAMES specify the set of windows to
  consider.

  MINIBUF nil or omitted means consider the minibuffer window only if the
  minibuffer is active.  MINIBUF t means consider the minibuffer window
  even if the minibuffer is not active.  Any other value means do not
  consider the minibuffer window even if the minibuffer is active.

  ALL-FRAMES nil or omitted means consider all windows on WINDOW's frame,
  plus the minibuffer window if specified by the MINIBUF argument.  If the
  minibuffer counts, consider all windows on all frames that share that
  minibuffer too.  The following non-nil values of ALL-FRAMES have special
  meanings:

  - t means consider all windows on all existing frames.

  - `visible' means consider all windows on all visible frames.

  - 0 (the number zero) means consider all windows on all visible and
    iconified frames.

  - A frame means consider all windows on that frame only.

  Anything else means consider all windows on WINDOW's frame and no
  others.

  If you use consistent values for MINIBUF and ALL-FRAMES, you can use
  `next-window' to iterate through the entire cycle of acceptable
  windows, eventually ending up back at the window you started with.
  `previous-window' traverses the same cycle, in the reverse order."
  )

(defun window-minibuffer-p (&optional window)
  "Return non-nil if WINDOW is a minibuffer window.
  If WINDOW is omitted or nil, it defaults to the selected window."
  (.mini-p (or window (selected-window))))

(defun frame-first-window (&optional frame-or-window)
  "Return the topmost, leftmost live window on FRAME-OR-WINDOW.
  If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
  Else if FRAME-OR-WINDOW denotes any window, return the first window of
  that window's frame.  If FRAME-OR-WINDOW denotes a live frame, return
  the first window of that frame."
  )

(defun window-scroll-bars (&optional window)
  "Get width and type of scroll bars of window WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window.
  Value is a list of the form (WIDTH COLS VERTICAL-TYPE HORIZONTAL-TYPE).
  If WIDTH is nil or TYPE is t, the window is using the frame's corresponding
  value."
  )

(defun window-dedicated-p (&optional window)
  "Return non-nil when WINDOW is dedicated to its buffer.
  More precisely, return the value assigned by the last call of
  `set-window-dedicated-p' for WINDOW.  Return nil if that function was
  never called with WINDOW as its argument, or the value set by that
  function was internally reset since its last call.  WINDOW defaults to
  the selected window.

  When a window is dedicated to its buffer, `display-buffer' will refrain
  from displaying another buffer in it.  `get-lru-window' and
  `get-largest-window' treat dedicated windows specially.
  `delete-windows-on', `replace-buffer-in-windows', `quit-window' and
  `kill-buffer' can delete a dedicated window and the containing frame.

  Functions like `set-window-buffer' may change the buffer displayed by a
  window, unless that window is \"strongly\" dedicated to its buffer, that
  is the value returned by `window-dedicated-p' is t."
  )

(defun selected-window ()
  "Return the selected window.
  The selected window is the window in which the standard cursor for
  selected windows appears and to which many commands apply."
  @(.selected-window ((el/fun 'selected-frame))))

(defun move-to-window-line (arg)
  "Position point relative to window.
  ARG nil means position point at center of window.
  Else, ARG specifies vertical position within the window;
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

(defun window-new-normal (&optional window)
  "Return new normal size of window WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window."
  )

(defun window-vscroll (&optional window pixels-p)
  "Return the amount by which WINDOW is scrolled vertically.
  If WINDOW is omitted or nil, it defaults to the selected window.
  Normally, value is a multiple of the canonical character height of WINDOW;
  optional second arg PIXELS-P means value is measured in pixels."
  )

(defun get-buffer-window (&optional buffer-or-name all-frames)
  "Return a window currently displaying BUFFER-OR-NAME, or nil if none.
  BUFFER-OR-NAME may be a buffer or a buffer name and defaults to
  the current buffer.

  The optional argument ALL-FRAMES specifies the frames to consider:

  - t means consider all windows on all existing frames.

  - `visible' means consider all windows on all visible frames.

  - 0 (the number zero) means consider all windows on all visible
      and iconified frames.

  - A frame means consider all windows on that frame only.

  Any other value of ALL-FRAMES means consider all windows on the
  selected frame and no others."
  )

(defun window-display-table (&optional window)
  "Return the display-table that WINDOW is using.
  WINDOW defaults to the selected window."
  )

(defun window-list (&optional frame minibuf window)
  "Return a list of windows on FRAME, starting with WINDOW.
  FRAME nil or omitted means use the selected frame.
  WINDOW nil or omitted means use the window selected within FRAME.
  MINIBUF t means include the minibuffer window, even if it isn't active.
  MINIBUF nil or omitted means include the minibuffer window only
  if it's active.
  MINIBUF neither nil nor t means never include the minibuffer window."
  (loop [w (or window (.root-window (or frame ((el/fun 'selected-frame)))))
         acc []]
    (if w
      (recur @(.next w)
             (concat
              (when-let [h @(.hchild w)] (window-list frame minibuf h))
              (when-let [v @(.vchild w)] (window-list frame minibuf v))
              (if (and (not minibuf) (.mini-p w))
                acc
                (conj acc w))))
      (cons/maybe-seq (reverse acc)))))

(defun frame-root-window (&optional frame-or-window)
  "Return the root window of FRAME-OR-WINDOW.
  If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
  With a frame argument, return that frame's root window.
  With a window argument, return the root window of that window's frame."
  (.root-window (or frame-or-window ((el/fun 'selected-frame)))))

(defun window-fringes (&optional window)
  "Get width of fringes of window WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window.
  Value is a list of the form (LEFT-WIDTH RIGHT-WIDTH OUTSIDE-MARGINS)."
  )

(defun window-parameters (&optional window)
  "Return the parameters of WINDOW and their values.
  WINDOW defaults to the selected window.  The return value is a list of
  elements of the form (PARAMETER . VALUE)."
  )

(defun window-at (x y &optional frame)
  "Return window containing coordinates X and Y on FRAME.
  FRAME must be a live frame and defaults to the selected one.
  The top left corner of the frame is considered to be row 0,
  column 0."
  )

(defun window-buffer (&optional window)
  "Return the buffer displayed in window WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window.
  Return nil for an internal window or a deleted window."
  @(.buffer (or window (selected-window))))

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

(defun window-list-1 (&optional window minibuf all-frames)
  "Return a list of all live windows.
  WINDOW specifies the first window to list and defaults to the selected
  window.

  Optional argument MINIBUF nil or omitted means consider the minibuffer
  window only if the minibuffer is active.  MINIBUF t means consider the
  minibuffer window even if the minibuffer is not active.  Any other value
  means do not consider the minibuffer window even if the minibuffer is
  active.

  Optional argument ALL-FRAMES nil or omitted means consider all windows
  on WINDOW's frame, plus the minibuffer window if specified by the
  MINIBUF argument.  If the minibuffer counts, consider all windows on all
  frames that share that minibuffer too.  The following non-nil values of
  ALL-FRAMES have special meanings:

  - t means consider all windows on all existing frames.

  - `visible' means consider all windows on all visible frames.

  - 0 (the number zero) means consider all windows on all visible and
    iconified frames.

  - A frame means consider all windows on that frame only.

  Anything else means consider all windows on WINDOW's frame and no
  others.

  If WINDOW is not on the list of windows returned, some other window will
  be listed first but no error is signaled."
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

(defun delete-window-internal (window)
  "Remove WINDOW from its frame.
  WINDOW defaults to the selected window.  Return nil.
  Signal an error when WINDOW is the only window on its frame."
  )

(defun window-hscroll (&optional window)
  "Return the number of columns by which WINDOW is scrolled from left margin.
  WINDOW must be a live window and defaults to the selected one."
  )

(defun window-prev-sibling (&optional window)
  "Return the previous sibling window of window WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window.
  Return nil if WINDOW has no previous sibling."
  )

(defun window-body-width (&optional window)
  "Return the width, in columns, of WINDOW's text area.
  If WINDOW is omitted or nil, it defaults to the selected window.
  Signal an error if the window is not live.

  The return value does not include any vertical dividers, fringe or
  marginal areas, or scroll bars.  On a graphical display, the width is
  expressed as an integer multiple of the default character width."
  )

(defun set-window-prev-buffers (window prev-buffers)
  "Set WINDOW's previous buffers to PREV-BUFFERS.
  WINDOW must be a live window and defaults to the selected one.

  PREV-BUFFERS should be a list of elements (BUFFER WINDOW-START POS),
  where BUFFER is a buffer, WINDOW-START is the start position of the
  window for that buffer, and POS is a window-specific point value."
  )

(defun window-prev-buffers (&optional window)
  "Return buffers previously shown in WINDOW.
  WINDOW must be a live window and defaults to the selected one.

  The return value is a list of elements (BUFFER WINDOW-START POS),
  where BUFFER is a buffer, WINDOW-START is the start position of the
  window for that buffer, and POS is a window-specific point value."
  )

(defun minibuffer-selected-window ()
  "Return the window which was selected when entering the minibuffer.
  Returns nil, if selected window is not a minibuffer window."
  )

(defun windowp (object)
  "Return t if OBJECT is a window and nil otherwise."
  (instance? Window object))

(defun set-window-dedicated-p (window flag)
  "Mark WINDOW as dedicated according to FLAG.
  WINDOW must be a live window and defaults to the selected one.  FLAG
  non-nil means mark WINDOW as dedicated to its buffer.  FLAG nil means
  mark WINDOW as non-dedicated.  Return FLAG.

  When a window is dedicated to its buffer, `display-buffer' will refrain
  from displaying another buffer in it.  `get-lru-window' and
  `get-largest-window' treat dedicated windows specially.
  `delete-windows-on', `replace-buffer-in-windows', `quit-window',
  `quit-restore-window' and `kill-buffer' can delete a dedicated window
  and the containing frame.

  As a special case, if FLAG is t, mark WINDOW as \"strongly\" dedicated to
  its buffer.  Functions like `set-window-buffer' may change the buffer
  displayed by a window, unless that window is strongly dedicated to its
  buffer.  If and when `set-window-buffer' displays another buffer in a
  window, it also makes sure that the window is no more dedicated."
  )

(defun window-text-height (&optional window)
  "Return the height in lines of the text display area of WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window.

  The returned height does not include the mode line, any header line,
  nor any partial-height lines at the bottom of the text area."
  )

(defun set-frame-selected-window (frame window &optional norecord)
  "Set selected window of FRAME to WINDOW.
  FRAME must be a live frame and defaults to the selected one.  If FRAME
  is the selected frame, this makes WINDOW the selected window.  Optional
  argument NORECORD non-nil means to neither change the order of recently
  selected windows nor the buffer list.  WINDOW must denote a live window.
  Return WINDOW."
  )

(defun window-inside-edges (&optional window)
  "Return a list of the edge coordinates of WINDOW.
  The list has the form (LEFT TOP RIGHT BOTTOM).
  TOP and BOTTOM count by lines, and LEFT and RIGHT count by columns,
  all relative to 0, 0 at top left corner of frame.

  RIGHT is one more than the rightmost column of WINDOW's text area.
  BOTTOM is one more than the bottommost row of WINDOW's text area.
  The inside edges do not include the space used by the WINDOW's scroll
  bar, display margins, fringes, header line, and/or mode line."
  )

(defun frame-selected-window (&optional frame-or-window)
  "Return the selected window of FRAME-OR-WINDOW.
  If omitted, FRAME-OR-WINDOW defaults to the currently selected frame.
  Else if FRAME-OR-WINDOW denotes any window, return the selected window
  of that window's frame.  If FRAME-OR-WINDOW denotes a live frame, return
  the selected window of that frame."
  )

(defun window-body-height (&optional window)
  "Return the height, in lines, of WINDOW's text area.
  If WINDOW is omitted or nil, it defaults to the selected window.
  Signal an error if the window is not live.

  The returned height does not include the mode line or header line.
  On a graphical display, the height is expressed as an integer multiple
  of the default character height.  If a line at the bottom of the text
  area is only partially visible, that counts as a whole line; to
  exclude partially-visible lines, use `window-text-height'."
  )

(defun previous-window (&optional window minibuf all-frames)
  "Return live window before WINDOW in the cyclic ordering of windows.
  WINDOW must be a live window and defaults to the selected one.  The
  optional arguments MINIBUF and ALL-FRAMES specify the set of windows to
  consider.

  MINIBUF nil or omitted means consider the minibuffer window only if the
  minibuffer is active.  MINIBUF t means consider the minibuffer window
  even if the minibuffer is not active.  Any other value means do not
  consider the minibuffer window even if the minibuffer is active.

  ALL-FRAMES nil or omitted means consider all windows on WINDOW's frame,
  plus the minibuffer window if specified by the MINIBUF argument.  If the
  minibuffer counts, consider all windows on all frames that share that
  minibuffer too.  The following non-nil values of ALL-FRAMES have special
  meanings:

  - t means consider all windows on all existing frames.

  - `visible' means consider all windows on all visible frames.

  - 0 (the number zero) means consider all windows on all visible and
    iconified frames.

  - A frame means consider all windows on that frame only.

  Anything else means consider all windows on WINDOW's frame and no
  others.

  If you use consistent values for MINIBUF and ALL-FRAMES, you can
  use `previous-window' to iterate through the entire cycle of
  acceptable windows, eventually ending up back at the window you
  started with.  `next-window' traverses the same cycle, in the
  reverse order."
  )

(defun set-window-new-normal (window &optional size)
  "Set new normal size of WINDOW to SIZE.
  Return SIZE.

  Note: This function does not operate on any child windows of WINDOW."
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
  If WINDOW is nil, the selected window is used.  Return POS.
  Optional third arg NOFORCE non-nil inhibits next redisplay from
  overriding motion of point in order to display at this exact start."
  )

(defun window-top-child (window)
  "Return the topmost child window of window WINDOW.
  Return nil if WINDOW is a live window (live windows have no children).
  Return nil if WINDOW is an internal window whose children form a
  horizontal combination."
  )

(defun window-new-total (&optional window)
  "Return the new total size of window WINDOW.
  If WINDOW is omitted or nil, it defaults to the selected window."
  )

(defun window-frame (window)
  "Return the frame that window WINDOW is on.
  If WINDOW is omitted or nil, it defaults to the selected window."
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

(defun window-left-child (window)
  "Return the leftmost child window of window WINDOW.
  Return nil if WINDOW is a live window (live windows have no children).
  Return nil if WINDOW is an internal window whose children form a
  vertical combination."
  )

(defun window-edges (&optional window)
  "Return a list of the edge coordinates of WINDOW.
  The list has the form (LEFT TOP RIGHT BOTTOM).
  TOP and BOTTOM count by lines, and LEFT and RIGHT count by columns,
  all relative to 0, 0 at top left corner of frame.

  RIGHT is one more than the rightmost column occupied by WINDOW.
  BOTTOM is one more than the bottommost row occupied by WINDOW.
  The edges include the space used by WINDOW's scroll bar, display
  margins, fringes, header line, and/or mode line.  For the edges of
  just the text area, use `window-inside-edges'."
  )

(defun window-resize-apply (frame &optional horizontal)
  "Apply requested size values for window-tree of FRAME.
  Optional argument HORIZONTAL omitted or nil means apply requested height
  values.  HORIZONTAL non-nil means apply requested width values.

  This function checks whether the requested values sum up to a valid
  window layout, recursively assigns the new sizes of all child windows
  and calculates and assigns the new start positions of these windows.

  Note: This function does not check any of `window-fixed-size-p',
  `window-min-height' or `window-min-width'.  All these checks have to
  be applied on the Elisp level."
  )

(defun compare-window-configurations (x y)
  "Compare two window configurations as regards the structure of windows.
  This function ignores details such as the values of point and mark
  and scrolling positions."
  )

(defun internal-temp-output-buffer-show (buf)
  "Internal function for `with-output-to-temp-buffer'.")

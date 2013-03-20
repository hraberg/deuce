(ns deuce.emacs.frame
  (:use [deuce.emacs-lisp :only (defun defvar) :as el])
  (:require [clojure.core :as c]
            [lanterna.screen :as s]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.buffer :as buffer]
            [deuce.emacs.data :as data]
            [deuce.emacs-lisp.cons :as cons]
            [deuce.emacs-lisp.globals :as globals])
  (:import [deuce.emacs.data Frame Window])
  (:refer-clojure :exclude []))

(defvar menu-bar-mode true
  "Non-nil if Menu-Bar mode is enabled.
  See the command `menu-bar-mode' for a description of this minor mode.
  Setting this variable directly does not take effect;
  either customize it (see the info node `Easy Customization')
  or call the function `menu-bar-mode'.

  You can customize this variable.")

(defvar delete-frame-functions nil
  "Functions to be run before deleting a frame.
  The functions are run with one arg, the frame to be deleted.
  See `delete-frame'.

  Note that functions in this list may be called just before the frame is
  actually deleted, or some time later (or even both when an earlier function
  in `delete-frame-functions' (indirectly) calls `delete-frame'
  recursively).")

(defvar make-pointer-invisible nil
  "If non-nil, make pointer invisible while typing.
  The pointer becomes visible again when the mouse is moved.

  You can customize this variable.")

(defvar window-system nil
  "Name of window system through which the selected frame is displayed.
  The value is a symbol:
   nil for a termcap frame (a character-only terminal),
   'x' for an Emacs frame that is really an X window,
   'w32' for an Emacs frame that is a window on MS-Windows display,
   'ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,
   'pc' for a direct-write MS-DOS frame.

  Use of this variable as a boolean is deprecated.  Instead,
  use `display-graphic-p' or any of the other `display-*-p'
  predicates which report frame's specific UI-related capabilities.")

(defvar terminal-frame nil
  "The initial frame-object, which represents Emacs's stdout.")

(defvar default-frame-alist nil
  "Alist of default values for frame creation.
  These may be set in your init file, like this:
    (setq default-frame-alist '((width . 80) (height . 55) (menu-bar-lines . 1)))
  These override values given in window system configuration data,
   including X Windows' defaults database.
  For values specific to the first Emacs frame, see `initial-frame-alist'.
  For window-system specific values, see `window-system-default-frame-alist'.
  For values specific to the separate minibuffer frame, see
   `minibuffer-frame-alist'.
  The `menu-bar-lines' element of the list controls whether new frames
   have menu bars; `menu-bar-mode' works by altering this element.
  Setting this variable does not affect existing frames, only new ones.

  You can customize this variable.")

(defvar focus-follows-mouse nil
  "Non-nil if window system changes focus when you move the mouse.
  You should set this variable to tell Emacs how your window manager
  handles focus, since there is no way in general for Emacs to find out
  automatically.  See also `mouse-autoselect-window'.

  You can customize this variable.")

(defvar mouse-highlight nil
  "If non-nil, clickable text is highlighted when mouse is over it.
  If the value is an integer, highlighting is only shown after moving the
  mouse, while keyboard input turns off the highlight even when the mouse
  is over the clickable text.  However, the mouse shape still indicates
  when the mouse is over clickable text.

  You can customize this variable.")

(defvar tool-bar-mode nil
  "Non-nil if Tool-Bar mode is enabled.
  See the command `tool-bar-mode' for a description of this minor mode.
  Setting this variable directly does not take effect;
  either customize it (see the info node `Easy Customization')
  or call the function `tool-bar-mode'.

  You can customize this variable.")

(defvar mouse-position-function nil
  "If non-nil, function to transform normal value of `mouse-position'.
  `mouse-position' calls this function, passing its usual return value as
  argument, and returns whatever this function returns.
  This abnormal hook exists for the benefit of packages like `xt-mouse.el'
  which need to do mouse handling at the Lisp level.")

(defvar default-frame-scroll-bars 'right
  "Default position of scroll bars on this window-system.")

(defvar default-minibuffer-frame globals/terminal-frame
  "Minibufferless frames use this frame's minibuffer.

  Emacs cannot create minibufferless frames unless this is set to an
  appropriate surrogate.

  Emacs consults this variable only when creating minibufferless
  frames; once the frame is created, it sticks with its assigned
  minibuffer, no matter what this variable is set to.  This means that
  this variable doesn't necessarily say anything meaningful about the
  current set of frames, or where the minibuffer is currently being
  displayed.

  This variable is local to the current terminal and cannot be buffer-local.")

(defn ^:private make-initial-frame []
  (let [allocate-window (ns-resolve 'deuce.emacs.window 'allocate-window)
        root-window (allocate-window false nil 0 1 10 9)
        selected-window (atom root-window)
        minibuffer-window (allocate-window true nil 0 9 10 1)
        terminal (atom nil)]
    (reset! (.next root-window) minibuffer-window)
    (reset! (.prev minibuffer-window) root-window)
    (Frame. "F1" root-window selected-window minibuffer-window terminal)))

(declare selected-frame frame-pixel-width frame-pixel-height)

(defun delete-frame (&optional frame force)
  "Delete FRAME, permanently eliminating it from use.
  FRAME defaults to the selected frame.

  A frame may not be deleted if its minibuffer is used by other frames.
  Normally, you may not delete a frame if all other frames are invisible,
  but if the second optional argument FORCE is non-nil, you may do so.

  This function runs `delete-frame-functions' before actually
  deleting the frame, unless the frame is a tooltip.
  The functions are run with one argument, the frame to be deleted."
  )

(defun lower-frame (&optional frame)
  "Send FRAME to the back, so it is occluded by any frames that overlap it.
  If you don't specify a frame, the selected frame is used.
  If Emacs is displaying on an ordinary terminal or some other device which
  doesn't support multiple overlapping frames, this function does nothing."
  )

(defun raise-frame (&optional frame)
  "Bring FRAME to the front, so it occludes any frames it overlaps.
  If FRAME is invisible or iconified, make it visible.
  If you don't specify a frame, the selected frame is used.
  If Emacs is displaying on an ordinary terminal or some other device which
  doesn't support multiple overlapping frames, this function selects FRAME."
  )

(defun frame-parameters (&optional frame)
  "Return the parameters-alist of frame FRAME.
  It is a list of elements of the form (PARM . VALUE), where PARM is a symbol.
  The meaningful PARMs depend on the kind of frame.
  If FRAME is omitted, return information on the currently selected frame."
  (let [frame (el/check-type 'framep (or frame (selected-frame)))]
    (list (cons/pair 'width (frame-pixel-width frame))
          (cons/pair 'height (frame-pixel-height frame))
          (cons/pair 'name (.name frame)))))

(defun frame-parameter (frame parameter)
  "Return FRAME's value for parameter PARAMETER.
  If FRAME is nil, describe the currently selected frame."
  ((el/fun 'assq) parameter (frame-parameters (or frame (selected-frame)))))

(defun framep (object)
  "Return non-nil if OBJECT is a frame.
  Value is:
    t for a termcap frame (a character-only terminal),
   'x' for an Emacs frame that is really an X window,
   'w32' for an Emacs frame that is a window on MS-Windows display,
   'ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,
   'pc' for a direct-write MS-DOS frame.
  See also `frame-live-p'."
  (instance? Frame object))

(defun frame-visible-p (frame)
  "Return t if FRAME is \"visible\" (actually in use for display).
  Return the symbol `icon' if FRAME is iconified or \"minimized\".
  Return nil if FRAME was made invisible, via `make-frame-invisible'.
  On graphical displays, invisible frames are not updated and are
  usually not displayed at all, even in a window system's \"taskbar\".

  If FRAME is a text-only terminal frame, this always returns t.
  Such frames are always considered visible, whether or not they are
  currently being displayed on the terminal."
  (framep frame))

(defun make-terminal-frame (parms)
  "Create an additional terminal frame, possibly on another terminal.
  This function takes one argument, an alist specifying frame parameters.

  You can create multiple frames on a single text-only terminal, but
  only one of them (the selected terminal frame) is actually displayed.

  In practice, generally you don't need to specify any parameters,
  except when you want to create a new frame on another terminal.
  In that case, the `tty' parameter specifies the device file to open,
  and the `tty-type' parameter specifies the terminal type.  Example:

     (make-terminal-frame '((tty . \"/dev/pts/5\") (tty-type . \"xterm\")))

  Note that changing the size of one terminal frame automatically
  affects all frames on the same terminal device."
  )

(defun modify-frame-parameters (frame alist)
  "Modify the parameters of frame FRAME according to ALIST.
  If FRAME is nil, it defaults to the selected frame.
  ALIST is an alist of parameters to change and their new values.
  Each element of ALIST has the form (PARM . VALUE), where PARM is a symbol.
  The meaningful PARMs depend on the kind of frame.
  Undefined PARMs are ignored, but stored in the frame's parameter list
  so that `frame-parameters' will return them.

  The value of frame parameter FOO can also be accessed
  as a frame-local binding for the variable FOO, if you have
  enabled such bindings for that variable with `make-variable-frame-local'.
  Note that this functionality is obsolete as of Emacs 22.2, and its
  use is not recommended.  Explicitly check for a frame-parameter instead."
  )

(defun handle-switch-frame (event)
  "Handle a switch-frame event EVENT.
  Switch-frame events are usually bound to this function.
  A switch-frame event tells Emacs that the window manager has requested
  that the user's events be directed to the frame mentioned in the event.
  This function selects the selected window of the frame of EVENT.

  If EVENT is frame object, handle it as if it were a switch-frame event
  to that frame."
  )

(defun make-frame-visible (&optional frame)
  "Make the frame FRAME visible (assuming it is an X window).
  If omitted, FRAME defaults to the currently selected frame."
  )

(defun frame-list ()
  "Return a list of all live frames."
  (alloc/list (selected-frame)))

(defun set-frame-size (frame cols rows)
  "Sets size of FRAME to COLS by ROWS, measured in characters."
  )

(defun window-system (&optional frame)
  "The name of the window system that FRAME is displaying through.
  The value is a symbol:
   nil for a termcap frame (a character-only terminal),
   'x' for an Emacs frame that is really an X window,
   'w32' for an Emacs frame that is a window on MS-Windows display,
   'ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,
   'pc' for a direct-write MS-DOS frame.

  FRAME defaults to the currently selected frame.

  Use of this function as a predicate is deprecated.  Instead,
  use `display-graphic-p' or any of the other `display-*-p'
  predicates which report frame's specific UI-related capabilities."
  nil)

(defun previous-frame (&optional frame miniframe)
  "Return the previous frame in the frame list before FRAME.
  It considers only frames on the same terminal as FRAME.
  By default, skip minibuffer-only frames.
  If omitted, FRAME defaults to the selected frame.
  If optional argument MINIFRAME is nil, exclude minibuffer-only frames.
  If MINIFRAME is a window, include only its own frame
  and any frame now using that window as the minibuffer.
  If MINIFRAME is `visible', include all visible frames.
  If MINIFRAME is 0, include all visible and iconified frames.
  Otherwise, include all frames."
  )

(defun set-mouse-pixel-position (frame x y)
  "Move the mouse pointer to pixel position (X,Y) in FRAME.
  The position is given in pixels, where (0, 0) is the upper-left corner
  of the frame, X is the horizontal offset, and Y is the vertical offset.

  Note, this is a no-op for an X frame that is not visible.
  If you have just created a frame, you must wait for it to become visible
  before calling this function on it, like this.
    (while (not (frame-visible-p frame)) (sleep-for .5))"
  )

(defun frame-pixel-height (&optional frame)
  "Return a FRAME's height in pixels.
  If FRAME is omitted, the selected frame is used.  The exact value
  of the result depends on the window-system and toolkit in use:

  In the Gtk+ version of Emacs, it includes only any window (including
  the minibuffer or echo area), mode line, and header line.  It does not
  include the tool bar or menu bar.

  With the Motif or Lucid toolkits, it also includes the tool bar (but
  not the menu bar).

  In a graphical version with no toolkit, it includes both the tool bar
  and menu bar.

  For a text-only terminal, it includes the menu bar.  In this case, the
  result is really in characters rather than pixels (i.e., is identical
  to `frame-height')."
  (if-let [s  @(.terminal (or frame (selected-frame)))]
    (second (s/get-size s))
    0))

(defun frame-live-p (object)
  "Return non-nil if OBJECT is a frame which has not been deleted.
  Value is nil if OBJECT is not a live frame.  If object is a live
  frame, the return value indicates what sort of terminal device it is
  displayed on.  See the documentation of `framep' for possible
  return values."
  (framep object))

(defun set-frame-width (frame cols &optional pretend)
  "Specify that the frame FRAME has COLS columns.
  Optional third arg non-nil means that redisplay should use COLS columns
  but that the idea of the actual width of the frame should not be changed."
  )

(defun select-frame (frame &optional norecord)
  "Select FRAME.
  Subsequent editing commands apply to its selected window.
  Optional argument NORECORD means to neither change the order of
  recently selected windows nor the buffer list.

  The selection of FRAME lasts until the next time the user does
  something to select a different frame, or until the next time
  this function is called.  If you are using a window system, the
  previously selected frame may be restored as the selected frame
  when returning to the command loop, because it still may have
  the window system's input focus.  On a text-only terminal, the
  next redisplay will display FRAME.

  This function returns FRAME, or nil if FRAME has been deleted."
  )

(defun next-frame (&optional frame miniframe)
  "Return the next frame in the frame list after FRAME.
  It considers only frames on the same terminal as FRAME.
  By default, skip minibuffer-only frames.
  If omitted, FRAME defaults to the selected frame.
  If optional argument MINIFRAME is nil, exclude minibuffer-only frames.
  If MINIFRAME is a window, include only its own frame
  and any frame now using that window as the minibuffer.
  If MINIFRAME is `visible', include all visible frames.
  If MINIFRAME is 0, include all visible and iconified frames.
  Otherwise, include all frames."
  )

(defun frame-pixel-width (&optional frame)
  "Return FRAME's width in pixels.
  For a terminal frame, the result really gives the width in characters.
  If FRAME is omitted, the selected frame is used."
  (if-let [s  @(.terminal (or frame (selected-frame)))]
    (first (s/get-size s))
    0))

(defun set-frame-height (frame lines &optional pretend)
  "Specify that the frame FRAME has LINES lines.
  Optional third arg non-nil means that redisplay should use LINES lines
  but that the idea of the actual height of the frame should not be changed."
  )

(defun frame-focus (frame)
  "Return the frame to which FRAME's keystrokes are currently being sent.
  This returns nil if FRAME's focus is not redirected.
  See `redirect-frame-focus'."
  )

(defun frame-pointer-visible-p (&optional frame)
  "Return t if the mouse pointer displayed on FRAME is visible.
  Otherwise it returns nil.  FRAME omitted or nil means the
  selected frame.  This is useful when `make-pointer-invisible' is set."
  )

(defun set-frame-position (frame xoffset yoffset)
  "Sets position of FRAME in pixels to XOFFSET by YOFFSET.
  This is actually the position of the upper left corner of the frame.
  Negative values for XOFFSET or YOFFSET are interpreted relative to
  the rightmost or bottommost possible position (that stays within the screen)."
  )

(defun iconify-frame (&optional frame)
  "Make the frame FRAME into an icon.
  If omitted, FRAME defaults to the currently selected frame."
  )

(defun make-frame-invisible (&optional frame force)
  "Make the frame FRAME invisible.
  If omitted, FRAME defaults to the currently selected frame.
  On graphical displays, invisible frames are not updated and are
  usually not displayed at all, even in a window system's \"taskbar\".

  Normally you may not make FRAME invisible if all other frames are invisible,
  but if the second optional argument FORCE is non-nil, you may do so.

  This function has no effect on text-only terminal frames.  Such frames
  are always considered visible, whether or not they are currently being
  displayed in the terminal."
  )

(defun frame-char-width (&optional frame)
  "Width in pixels of characters in the font in frame FRAME.
  If FRAME is omitted, the selected frame is used.
  On a graphical screen, the width is the standard width of the default font.
  For a terminal screen, the value is always 1."
  )

(defun mouse-position ()
  "Return a list (FRAME X . Y) giving the current mouse frame and position.
  The position is given in character cells, where (0, 0) is the
  upper-left corner of the frame, X is the horizontal offset, and Y is
  the vertical offset.
  If Emacs is running on a mouseless terminal or hasn't been programmed
  to read the mouse position, it returns the selected frame for FRAME
  and nil for X and Y.
  If `mouse-position-function' is non-nil, `mouse-position' calls it,
  passing the normal return value to that function as an argument,
  and returns whatever that function returns."
  )

(defun set-mouse-position (frame x y)
  "Move the mouse pointer to the center of character cell (X,Y) in FRAME.
  Coordinates are relative to the frame, not a window,
  so the coordinates of the top left character in the frame
  may be nonzero due to left-hand scroll bars or the menu bar.

  The position is given in character cells, where (0, 0) is the
  upper-left corner of the frame, X is the horizontal offset, and Y is
  the vertical offset.

  This function is a no-op for an X frame that is not visible.
  If you have just created a frame, you must wait for it to become visible
  before calling this function on it, like this.
    (while (not (frame-visible-p frame)) (sleep-for .5))"
  )

(defun selected-frame ()
  "Return the frame that is now selected."
  globals/terminal-frame)

(defun redirect-frame-focus (frame &optional focus-frame)
  "Arrange for keystrokes typed at FRAME to be sent to FOCUS-FRAME.
  In other words, switch-frame events caused by events in FRAME will
  request a switch to FOCUS-FRAME, and `last-event-frame' will be
  FOCUS-FRAME after reading an event typed at FRAME.

  If FOCUS-FRAME is omitted or nil, any existing redirection is
  canceled, and the frame again receives its own keystrokes.

  Focus redirection is useful for temporarily redirecting keystrokes to
  a surrogate minibuffer frame when a frame doesn't have its own
  minibuffer window.

  A frame's focus redirection can be changed by `select-frame'.  If frame
  FOO is selected, and then a different frame BAR is selected, any
  frames redirecting their focus to FOO are shifted to redirect their
  focus to BAR.  This allows focus redirection to work properly when the
  user switches from one frame to another using `select-window'.

  This means that a frame whose focus is redirected to itself is treated
  differently from a frame whose focus is redirected to nil; the former
  is affected by `select-frame', while the latter is not.

  The redirection lasts until `redirect-frame-focus' is called to change it."
  )

(defun tool-bar-pixel-width (&optional frame)
  "Return width in pixels of FRAME's tool bar.
  The result is greater than zero only when the tool bar is on the left
  or right side of FRAME.  If FRAME is omitted, the selected frame is
  used."
  )

(defun visible-frame-list ()
  "Return a list of all frames now \"visible\" (being updated)."
  (frame-list))

(defun frame-char-height (&optional frame)
  "Height in pixels of a line in the font in frame FRAME.
  If FRAME is omitted, the selected frame is used.
  For a terminal frame, the value is always 1."
  )

(defun mouse-pixel-position ()
  "Return a list (FRAME X . Y) giving the current mouse frame and position.
  The position is given in pixel units, where (0, 0) is the
  upper-left corner of the frame, X is the horizontal offset, and Y is
  the vertical offset.
  If Emacs is running on a mouseless terminal or hasn't been programmed
  to read the mouse position, it returns the selected frame for FRAME
  and nil for X and Y."
  )

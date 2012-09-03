(ns
 deuce.emacs.dispnew
 (use [deuce.emacs-lisp :only (defun defvar)])
 (require [clojure.core :as c])
 (:refer-clojure :exclude []))

(defvar no-redraw-on-reenter nil
  "Non-nil means no need to redraw entire frame after suspending.
  A non-nil value is useful if the terminal can automatically preserve
  Emacs's frame display when you reenter Emacs.
  It is up to you to set this variable if your terminal can do that.
  
  You can customize this variable.")

(defvar cursor-in-echo-area nil
  "Non-nil means put cursor in minibuffer, at end of any message there.")

(defvar standard-display-table nil
  "Display table to use for buffers that specify none.
  See `buffer-display-table' for more information.")

(defvar visible-bell nil
  "Non-nil means try to flash the frame to represent a bell.
  
  See also `ring-bell-function'.
  
  You can customize this variable.")

(defvar redisplay-dont-pause nil
  "Non-nil means display update isn't paused when input is detected.")

(defvar redisplay-preemption-period nil
  "Period in seconds between checking for input during redisplay.
  This has an effect only if `redisplay-dont-pause' is nil; in that
  case, arriving input preempts redisplay until the input is processed.
  If the value is nil, redisplay is never preempted.")

(defvar window-system-version nil
  "The version number of the window system in use.
  For X windows, this is 11.")

(defvar initial-window-system nil
  "Name of the window system that Emacs uses for the first frame.
  The value is a symbol:
   nil for a termcap frame (a character-only terminal),
   'x' for an Emacs frame that is really an X window,
   'w32' for an Emacs frame that is a window on MS-Windows display,
   'ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,
   'pc' for a direct-write MS-DOS frame.
  
  Use of this variable as a boolean is deprecated.  Instead,
  use `display-graphic-p' or any of the other `display-*-p'
  predicates which report frame's specific UI-related capabilities.")

(defvar baud-rate nil
  "The output baud rate of the terminal.
  On most systems, changing this value will affect the amount of padding
  and the other strategic decisions made during redisplay.
  
  You can customize this variable.")

(defvar glyph-table nil
  "Table defining how to output a glyph code to the frame.
  If not nil, this is a vector indexed by glyph code to define the glyph.
  Each element can be:
   integer: a glyph code which this glyph is an alias for.
   string: output this glyph using that string (not impl. in X windows).
   nil: this glyph mod 524288 is the code of a character to output,
      and this glyph / 524288 is the face number (see `face-id') to use
      while outputting it.")

(defvar inverse-video nil
  "Non-nil means invert the entire frame display.
  This means everything is in inverse video which otherwise would not be.
  
  You can customize this variable.")

(defun open-termscript (file)
  "Start writing all terminal output to FILE as well as the terminal.
  FILE = nil means just close any termscript file currently open."
  )

(defun ding (&optional arg)
  "Beep, or flash the screen.
  Also, unless an argument is given,
  terminate any keyboard macro currently executing."
  )

(defun internal-show-cursor (window show)
  "Set the cursor-visibility flag of WINDOW to SHOW.
  WINDOW nil means use the selected window.  SHOW non-nil means
  show a cursor in WINDOW in the next redisplay.  SHOW nil means
  don't show a cursor."
  )

(defun redraw-frame (frame)
  "Clear frame FRAME and output again what is supposed to appear on it."
  )

(defun frame-or-buffer-changed-p (&optional variable)
  "Return non-nil if the frame and buffer state appears to have changed.
  VARIABLE is a variable name whose value is either nil or a state vector
  that will be updated to contain all frames and buffers,
  aside from buffers whose names start with space,
  along with the buffers' read-only and modified flags.  This allows a fast
  check to see whether buffer menus might need to be recomputed.
  If this function returns non-nil, it updates the internal vector to reflect
  the current state.
  
  If VARIABLE is nil, an internal variable is used.  Users should not
  pass nil for VARIABLE."
  )

(defun redisplay (&optional force)
  "Perform redisplay.
  Optional arg FORCE, if non-nil, prevents redisplay from being
  preempted by arriving input, even if `redisplay-dont-pause' is nil.
  If `redisplay-dont-pause' is non-nil (the default), redisplay is never
  preempted by arriving input, so FORCE does nothing.
  
  Return t if redisplay was performed, nil if redisplay was preempted
  immediately by pending input."
  )

(defun internal-show-cursor-p (&optional window)
  "Value is non-nil if next redisplay will display a cursor in WINDOW.
  WINDOW nil or omitted means report on the selected window."
  )

(defun last-nonminibuffer-frame ()
  "Value is last nonminibuffer frame."
  )

(defun send-string-to-terminal (string &optional terminal)
  "Send STRING to the terminal without alteration.
  Control characters in STRING will have terminal-dependent effects.
  
  Optional parameter TERMINAL specifies the tty terminal device to use.
  It may be a terminal object, a frame, or nil for the terminal used by
  the currently selected frame.  In batch mode, STRING is sent to stdout
  when TERMINAL is nil."
  )

(defun redraw-display ()
  "Clear and redisplay all visible frames."
  )

(defun sleep-for (seconds &optional milliseconds)
  "Pause, without updating display, for SECONDS seconds.
  SECONDS may be a floating-point value, meaning that you can wait for a
  fraction of a second.  Optional second arg MILLISECONDS specifies an
  additional wait period, in milliseconds; this may be useful if your
  Emacs was built without floating point support.
  (Not all operating systems support waiting for a fraction of a second.)"
  )

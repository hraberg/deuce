(ns
 deuce.emacs.dispnew
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

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

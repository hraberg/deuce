(ns emacs.dispnew (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun ding (&optional arg)
  "Beep, or flash the screen.
  Also, unless an argument is given,
  terminate any keyboard macro currently executing."
  )

(defun internal-show-cursor (window show)
  "Set the cursor-visibility flag of WINDOW to SHOW.
  WINDOW nil means use the selected window.  SHOW non-nil means
  show a cursor in WINDOW in the next redisplay.  SHOW nil means
  don't show a cursor.log10 is a built-in function in `C source code'."
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
  the current state."
  )

(defun redisplay (&optional force)
  "Perform redisplay if no input is available.
  If optional arg FORCE is non-nil or `redisplay-dont-pause' is non-nil,
  perform a full redisplay even if input is available.
  Return t if redisplay was performed, nil otherwise."
  )

(defun internal-show-cursor-p (&optional window)
  "Value is non-nil if next redisplay will display a cursor in WINDOW.
  WINDOW nil or omitted means report on the selected window.make-variable-buffer-local is an interactive built-in function in `C
  source code'."
  )

(defun last-nonminibuffer-frame ()
  "Value is last nonminibuffer frame."
  )

(defun send-string-to-terminal (string &optional terminal)
  "Send STRING to the terminal without alteration.
  Control characters in STRING will have terminal-dependent effects."
  )

(defun sleep-for (seconds &optional milliseconds)
  "Pause, without updating display, for SECONDS seconds.
  SECONDS may be a floating-point value, meaning that you can wait for a
  fraction of a second.  Optional second arg MILLISECONDS specifies an
  additional wait period, in milliseconds; this may be useful if your
  Emacs was built without floating point support.
  (Not all operating systems support waiting for a fraction of a second.)"
  )

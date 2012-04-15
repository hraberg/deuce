(ns emacs.dispnew (use [deuce.core]) (:refer-clojure :only []))

(defun ding (&optional arg)
  "Beep, or flash the screen.\nAlso, unless an argument is given,\n"
  )

(defun internal-show-cursor (window show)
  "Set the cursor-visibility flag of WINDOW to SHOW.\nWINDOW nil means use the selected window.  SHOW non-nil means\nshow a cursor in WINDOW in the next redisplay.  SHOW nil means\n"
  )

(defun redraw-frame (frame)
  )

(defun frame-or-buffer-changed-p (&optional variable)
  "Return non-nil if the frame and buffer state appears to have changed.\nVARIABLE is a variable name whose value is either nil or a state vector\nthat will be updated to contain all frames and buffers,\naside from buffers whose names start with space,\nalong with the buffers' read-only and modified flags.  This allows a fast\ncheck to see whether buffer menus might need to be recomputed.\nIf this function returns non-nil, it updates the internal vector to reflect\nthe current state."
  )

(defun redisplay (&optional force)
  "Perform redisplay if no input is available.\nIf optional arg FORCE is non-nil or `redisplay-dont-pause' is non-nil,\nperform a full redisplay even if input is available.\n"
  )

(defun internal-show-cursor-p (&optional window)
  "Value is non-nil if next redisplay will display a cursor in WINDOW.\nWINDOW nil or omitted means report on the selected window.make-variable-buffer-local is an interactive built-in function in `C\nsource code'."
  )

(defun last-nonminibuffer-frame ()
  )

(defun send-string-to-terminal (string &optional terminal)
  "Send STRING to the terminal without alteration.\nControl characters in STRING will have terminal-dependent effects."
  )

(defun sleep-for (seconds &optional milliseconds)
  "Pause, without updating display, for SECONDS seconds.\nSECONDS may be a floating-point value, meaning that you can wait for a\nfraction of a second.  Optional second arg MILLISECONDS specifies an\nadditional wait period, in milliseconds; this may be useful if your\nEmacs was built without floating point support.\n"
  )

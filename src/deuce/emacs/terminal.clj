(ns
 deuce.emacs.terminal
 (use [deuce.emacs-lisp :only (defun defvar)])
 (require [clojure.core :as c])
 (:refer-clojure :exclude []))

(defvar delete-terminal-functions nil
  "Special hook run when a terminal is deleted.
  Each function is called with argument, the terminal.
  This may be called just before actually deleting the terminal,
  or some time later.")

(defvar ring-bell-function nil
  "Non-nil means call this function to ring the bell.
  The function should accept no arguments.")

(defun terminal-list ()
  "Return a list of all terminal devices."
  )

(defun terminal-parameter (terminal parameter)
  "Return TERMINAL's value for parameter PARAMETER.
  TERMINAL can be a terminal object, a frame, or nil (meaning the
  selected frame's terminal)."
  )

(defun terminal-live-p (object)
  "Return non-nil if OBJECT is a terminal which has not been deleted.
  Value is nil if OBJECT is not a live display terminal.
  If object is a live display terminal, the return value indicates what
  sort of output terminal it uses.  See the documentation of `framep' for
  possible return values."
  )

(defun frame-terminal (&optional frame)
  "Return the terminal that FRAME is displayed on.
  If FRAME is nil, the selected frame is used.
  
  The terminal device is represented by its integer identifier."
  )

(defun delete-terminal (&optional terminal force)
  "Delete TERMINAL by deleting all frames on it and closing the terminal.
  TERMINAL may be a terminal object, a frame, or nil (meaning the
  selected frame's terminal).
  
  Normally, you may not delete a display if all other displays are suspended,
  but if the second argument FORCE is non-nil, you may do so."
  )

(defun set-terminal-parameter (terminal parameter value)
  "Set TERMINAL's value for parameter PARAMETER to VALUE.
  Return the previous value of PARAMETER.
  
  TERMINAL can be a terminal object, a frame or nil (meaning the
  selected frame's terminal)."
  )

(defun terminal-parameters (&optional terminal)
  "Return the parameter-alist of terminal TERMINAL.
  The value is a list of elements of the form (PARM . VALUE), where PARM
  is a symbol.
  
  TERMINAL can be a terminal object, a frame, or nil (meaning the
  selected frame's terminal)."
  )

(defun terminal-name (&optional terminal)
  "Return the name of the terminal device TERMINAL.
  It is not guaranteed that the returned value is unique among opened devices.
  
  TERMINAL may be a terminal object, a frame, or nil (meaning the
  selected frame's terminal)."
  )

(ns emacs.terminal (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun terminal-list ()
  )

(defun terminal-parameter (terminal parameter)
  "Return TERMINAL's value for parameter PARAMETER.\nTERMINAL can be a terminal object, a frame, or nil (meaning the\n"
  )

(defun terminal-live-p (object)
  "Return non-nil if OBJECT is a terminal which has not been deleted.\nValue is nil if OBJECT is not a live display terminal.\nIf object is a live display terminal, the return value indicates what\nsort of output terminal it uses.  See the documentation of `framep' for\n"
  )

(defun frame-terminal (&optional frame)
  "Return the terminal that FRAME is displayed on.\nIf FRAME is nil, the selected frame is used."
  )

(defun delete-terminal (&optional terminal force)
  "Delete TERMINAL by deleting all frames on it and closing the terminal.\nTERMINAL may be a terminal object, a frame, or nil (meaning the\nselected frame's terminal)."
  )

(defun set-terminal-parameter (terminal parameter value)
  "Set TERMINAL's value for parameter PARAMETER to VALUE.\nReturn the previous value of PARAMETER."
  )

(defun terminal-parameters (&optional terminal)
  "Return the parameter-alist of terminal TERMINAL.\nThe value is a list of elements of the form (PARM . VALUE), where PARM\nis a symbol."
  )

(defun terminal-name (&optional terminal)
  "Return the name of the terminal device TERMINAL.\nIt is not guaranteed that the returned value is unique among opened devices."
  )

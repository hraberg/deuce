(ns
 deuce.emacs.term
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun controlling-tty-p (&optional terminal)
  "Return non-nil if TERMINAL is the controlling tty of the Emacs process."
  )

(defun tty-display-color-p (&optional terminal)
  "Return non-nil if the tty device TERMINAL can display colors."
  )

(defun tty-no-underline (&optional terminal)
  "Declare that the tty used by TERMINAL does not handle underlining.
  This is used to override the terminfo data, for certain terminals that
  do not really do underlining, but say that they do.  This function has
  no effect if used on a non-tty terminal."
  )

(defun tty-type (&optional terminal)
  "Return the type of the tty device that TERMINAL uses.
  Returns nil if TERMINAL is not on a tty device."
  )

(defun tty-display-color-cells (&optional terminal)
  "Return the number of colors supported by the tty device TERMINAL."
  )

(defun resume-tty (&optional tty)
  "Resume the previously suspended terminal device TTY.
  The terminal is opened and reinitialized.  Frames that are on the
  suspended terminal are revived."
  )

(defun suspend-tty (&optional tty)
  "Suspend the terminal device TTY."
  )

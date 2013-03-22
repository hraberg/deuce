(ns deuce.emacs.term
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c]
            [lanterna.screen :as s]
            [deuce.emacs.eval :as eval]
            [deuce.emacs.terminal :as terminal])
  (:refer-clojure :exclude []))

(defvar suspend-tty-functions nil
  "Functions to be run after suspending a tty.
  The functions are run with one argument, the terminal object to be suspended.
  See `suspend-tty'.")

(defvar resume-tty-functions nil
  "Functions to be run after resuming a tty.
  The functions are run with one argument, the terminal object that was revived.
  See `resume-tty'.")

(defvar visible-cursor nil
  "Non-nil means to make the cursor very visible.
  This only has an effect when running in a text terminal.
  What means \"very visible\" is up to your terminal.  It may make the cursor
  bigger, or it may make it blink, or it may do nothing at all.

  You can customize this variable.")

(defvar system-uses-terminfo nil
  "Non-nil means the system uses terminfo rather than termcap.
  This variable can be used by terminal emulator packages.")

(defun controlling-tty-p (&optional terminal)
  "Return non-nil if TERMINAL is the controlling tty of the Emacs process.

  TERMINAL can be a terminal object, a frame, or nil (meaning the
  selected frame's terminal).  This function always returns nil if
  TERMINAL is not on a tty device."
  )

(defun tty-display-color-p (&optional terminal)
  "Return non-nil if the tty device TERMINAL can display colors.

  TERMINAL can be a terminal object, a frame, or nil (meaning the
  selected frame's terminal).  This function always returns nil if
  TERMINAL does not refer to a text-only terminal."
  nil)

(defun tty-no-underline (&optional terminal)
  "Declare that the tty used by TERMINAL does not handle underlining.
  This is used to override the terminfo data, for certain terminals that
  do not really do underlining, but say that they do.  This function has
  no effect if used on a non-tty terminal.

  TERMINAL can be a terminal object, a frame or nil (meaning the
  selected frame's terminal).  This function always returns nil if
  TERMINAL does not refer to a text-only terminal."
  )

(defun tty-type (&optional terminal)
  "Return the type of the tty device that TERMINAL uses.
  Returns nil if TERMINAL is not on a tty device.

  TERMINAL can be a terminal object, a frame, or nil (meaning the
  selected frame's terminal)."
  "lanterna")

(defun tty-display-color-cells (&optional terminal)
  "Return the number of colors supported by the tty device TERMINAL.

  TERMINAL can be a terminal object, a frame, or nil (meaning the
  selected frame's terminal).  This function always returns 0 if
  TERMINAL does not refer to a text-only terminal."
  16)

(defun resume-tty (&optional tty)
  "Resume the previously suspended terminal device TTY.
  The terminal is opened and reinitialized.  Frames that are on the
  suspended terminal are revived.

  It is an error to resume a terminal while another terminal is active
  on the same device.

  This function runs `resume-tty-functions' after resuming the terminal.
  The functions are run with one arg, the id of the resumed terminal
  device.

  `resume-tty' does nothing if it is called on a device that is not
  suspended.

  TTY may be a terminal object, a frame, or nil (meaning the selected
  frame's terminal)."
  (when-let [terminal (terminal/frame-terminal)]
    (s/start terminal)
    (s/clear terminal)
    (s/redraw terminal)
    (eval/run-hook-with-args 'resume-tty-functions terminal)))

(defun suspend-tty (&optional tty)
  "Suspend the terminal device TTY.

  The device is restored to its default state, and Emacs ceases all
  access to the tty device.  Frames that use the device are not deleted,
  but input is not read from them and if they change, their display is
  not updated.

  TTY may be a terminal object, a frame, or nil for the terminal device
  of the currently selected frame.

  This function runs `suspend-tty-functions' after suspending the
  device.  The functions are run with one arg, the id of the suspended
  terminal device.

  `suspend-tty' does nothing if it is called on a device that is already
  suspended.

  A suspended tty may be resumed by calling `resume-tty' on it."
  (when-let [terminal (terminal/frame-terminal)]
    (s/clear terminal)
    (s/redraw terminal)
    (s/stop terminal)
    (eval/run-hook-with-args 'suspend-tty-functions terminal)))

(ns deuce.emacs.cmds
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c])
  (:refer-clojure :exclude []))

(defvar post-self-insert-hook nil
  "Hook run at the end of `self-insert-command'.
  This is run after inserting the character.")

(defun forward-line (&optional n)
  "Move N lines forward (backward if N is negative).
  Precisely, if point is on line I, move to the start of line I + N
  (\"start of line\" in the logical order).
  If there isn't room, go as far as possible (no error).
  Returns the count of lines left to move.  If moving forward,
  that is N - number of lines moved; if backward, N + number moved.
  With positive N, a non-empty line at the end counts as one line
  successfully moved (for the return value)."
  )

(defun forward-char (&optional n)
  "Move point N characters forward (backward if N is negative).
  On reaching end or beginning of buffer, stop and signal error.

  Depending on the bidirectional context, the movement may be to the
  right or to the left on the screen.  This is in contrast with
  <right>, which see."
  )

(defun forward-point (n)
  "This function is obsolete since 23.1;
  use (+ (point) N) instead.

  Return buffer position N characters after (before if N negative) point."
  )

(defun self-insert-command (n)
  "Insert the character you type.
  Whichever character you type to run this command is inserted.
  Before insertion, `expand-abbrev' is executed if the inserted character does
  not have word syntax and the previous character in the buffer does.
  After insertion, the value of `auto-fill-function' is called if the
  `auto-fill-chars' table has a non-nil value for the inserted character.
  At the end, it runs `post-self-insert-hook'."
  )

(defun backward-char (&optional n)
  "Move point N characters backward (forward if N is negative).
  On attempt to pass beginning or end of buffer, stop and signal error.

  Depending on the bidirectional context, the movement may be to the
  right or to the left on the screen.  This is in contrast with
  <left>, which see."
  )

(defun beginning-of-line (&optional n)
  "Move point to beginning of current line (in the logical order).
  With argument N not nil or 1, move forward N - 1 lines first.
  If point reaches the beginning or end of buffer, it stops there.

  This function constrains point to the current field unless this moves
  point to a different line than the original, unconstrained result.
  If N is nil or 1, and a front-sticky field starts at point, the point
  does not move.  To ignore field boundaries bind
  `inhibit-field-text-motion' to t, or use the `forward-line' function
  instead.  For instance, `(forward-line 0)' does the same thing as
  `(beginning-of-line)', except that it ignores field boundaries."
  )

(defun delete-char (n &optional killflag)
  "Delete the following N characters (previous if N is negative).
  Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).
  Interactively, N is the prefix arg, and KILLFLAG is set if
  N was explicitly specified.

  The command `delete-forward-char' is preferable for interactive use."
  )

(defun end-of-line (&optional n)
  "Move point to end of current line (in the logical order).
  With argument N not nil or 1, move forward N - 1 lines first.
  If point reaches the beginning or end of buffer, it stops there.
  To ignore intangibility, bind `inhibit-point-motion-hooks' to t.

  This function constrains point to the current field unless this moves
  point to a different line than the original, unconstrained result.  If
  N is nil or 1, and a rear-sticky field ends at point, the point does
  not move.  To ignore field boundaries bind `inhibit-field-text-motion'
  to t."
  )

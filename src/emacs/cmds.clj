(ns emacs.cmds (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun forward-line (&optional n)
  "Move N lines forward (backward if N is negative).
  Precisely, if point is on line I, move to the start of line I + N.
  If there isn't room, go as far as possible (no error).
  Returns the count of lines left to move.  If moving forward,
  that is N - number of lines moved; if backward, N + number moved.
  With positive N, a non-empty line at the end counts as one line
    successfully moved (for the return value)."
  )

(defun delete-backward-char (n &optional killflag)
  "Delete the previous N characters (following if N is negative).
  Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).
  Interactively, N is the prefix arg, and KILLFLAG is set if
  N was explicitly specified.
  This is meant for interactive use only; from Lisp, better use `delete-char'
  with a negated argument."
  )

(defun forward-char (&optional n)
  "Move point right N characters (left if N is negative).
  On reaching end of buffer, stop and signal error."
  )

(defun forward-point (n)
  "This function is obsolete since 23.1;
  use (+ (point) N) instead."
  )

(defun self-insert-command (n)
  "Insert the character you type.
  Whichever character you type to run this command is inserted.
  Before insertion, `expand-abbrev' is executed if the inserted character does
  not have word syntax and the previous character in the buffer does.
  After insertion, the value of `auto-fill-function' is called if the
  `auto-fill-chars' table has a non-nil value for the inserted character."
  )

(defun backward-char (&optional n)
  "Move point left N characters (right if N is negative).
  On attempt to pass beginning or end of buffer, stop and signal error."
  )

(defun beginning-of-line (&optional n)
  "Move point to beginning of current line.
  With argument N not nil or 1, move forward N - 1 lines first.
  If point reaches the beginning or end of buffer, it stops there."
  )

(defun delete-char (n &optional killflag)
  "Delete the following N characters (previous if N is negative).
  Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).
  Interactively, N is the prefix arg, and KILLFLAG is set if
  N was explicitly specified."
  )

(defun end-of-line (&optional n)
  "Move point to end of current line.
  With argument N not nil or 1, move forward N - 1 lines first.
  If point reaches the beginning or end of buffer, it stops there.
  To ignore intangibility, bind `inhibit-point-motion-hooks' to t."
  )

(ns
 emacs.xdisp
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun format-mode-line (format &optional face window buffer)
  "Format a string out of a mode line format specification.
  First arg FORMAT specifies the mode line format (see `mode-line-format'
  for details) to use."
  )

(defun invisible-p (pos-or-prop)
  "Non-nil if the property makes the text invisible.
  POS-OR-PROP can be a marker or number, in which case it is taken to be
  a position in the current buffer and the value of the `invisible' property
  is checked; or it can be some other value, which is then presumed to be the
  value of the `invisible' property of the text of interest.
  The non-nil value returned can be t for truly invisible text or something
  else if the text is replaced by an ellipsis."
  )

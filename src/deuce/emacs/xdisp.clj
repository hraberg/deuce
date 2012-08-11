(ns
 deuce.emacs.xdisp
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun current-bidi-paragraph-direction (&optional buffer)
  "Return paragraph direction at point in BUFFER.
  Value is either `left-to-right' or `right-to-left'.
  If BUFFER is omitted or nil, it defaults to the current buffer.
  
  Paragraph direction determines how the text in the paragraph is displayed.
  In left-to-right paragraphs, text begins at the left margin of the window
  and the reading direction is generally left to right.  In right-to-left
  paragraphs, text begins at the right margin and is read from right to left.
  
  See also `bidi-paragraph-direction'."
  )

(defun format-mode-line (format &optional face window buffer)
  "Format a string out of a mode line format specification.
  First arg FORMAT specifies the mode line format (see `mode-line-format'
  for details) to use.
  
  By default, the format is evaluated for the currently selected window.
  
  Optional second arg FACE specifies the face property to put on all
  characters for which no face is specified.  The value nil means the
  default face.  The value t means whatever face the window's mode line
  currently uses (either `mode-line' or `mode-line-inactive',
  depending on whether the window is the selected window or not).
  An integer value means the value string has no text
  properties.
  
  Optional third and fourth args WINDOW and BUFFER specify the window
  and buffer to use as the context for the formatting (defaults
  are the selected window and the WINDOW's buffer)."
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

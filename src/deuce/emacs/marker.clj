(ns
 deuce.emacs.marker
 (:use [deuce.emacs-lisp :only (defun defvar)])
 (:refer-clojure :exclude []))

(defvar byte-debug-flag nil
  "Non-nil enables debugging checks in byte/char position conversions.")

(defun marker-position (marker)
  "Return the position MARKER points at, as a character number.
  Returns nil if MARKER points nowhere."
  )

(defun buffer-has-markers-at (position)
  "Return t if there are markers pointing at POSITION in the current buffer."
  )

(defun copy-marker (&optional marker type)
  "Return a new marker pointing at the same place as MARKER.
  If argument is a number, makes a new marker pointing
  at that position in the current buffer.
  If MARKER is not specified, the new marker does not point anywhere.
  The optional argument TYPE specifies the insertion type of the new marker;
  see `marker-insertion-type'."
  )

(defun marker-insertion-type (marker)
  "Return insertion type of MARKER: t if it stays after inserted text.
  The value nil means the marker stays before text inserted there."
  )

(defun set-marker-insertion-type (marker type)
  "Set the insertion-type of MARKER to TYPE.
  If TYPE is t, it means the marker advances when you insert text at it.
  If TYPE is nil, it means the marker stays behind when you insert text at it."
  )

(defun marker-buffer (marker)
  "Return the buffer that MARKER points into, or nil if none.
  Returns nil if MARKER points into a dead buffer."
  )

(defun set-marker (marker position &optional buffer)
  "Position MARKER before character number POSITION in BUFFER.
  BUFFER defaults to the current buffer.
  If POSITION is nil, makes marker point nowhere.
  Then it no longer slows down editing in any buffer.
  Returns MARKER."
  )

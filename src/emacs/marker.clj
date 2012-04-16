(ns emacs.marker (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun marker-position (marker)
  "Return the position MARKER points at, as a character number."
  )

(defun buffer-has-markers-at (position)
  )

(defun copy-marker (marker &optional type)
  "Return a new marker pointing at the same place as MARKER.
  If argument is a number, makes a new marker pointing
  at that position in the current buffer.
  The optional argument TYPE specifies the insertion type of the new marker;"
  )

(defun marker-insertion-type (marker)
  "Return insertion type of MARKER: t if it stays after inserted text."
  )

(defun set-marker-insertion-type (marker type)
  "Set the insertion-type of MARKER to TYPE.
  If TYPE is t, it means the marker advances when you insert text at it."
  )

(defun marker-buffer (marker)
  "Return the buffer that MARKER points into, or nil if none."
  )

(defun set-marker (marker position &optional buffer)
  "Position MARKER before character number POSITION in BUFFER.
  BUFFER defaults to the current buffer.
  If POSITION is nil, makes marker point nowhere.
  Then it no longer slows down editing in any buffer."
  )

(ns emacs.marker (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun marker-position (marker)
  "Return the position MARKER points at, as a character number.\n"
  )

(defun buffer-has-markers-at (position)
  )

(defun copy-marker (marker &optional type)
  "Return a new marker pointing at the same place as MARKER.\nIf argument is a number, makes a new marker pointing\nat that position in the current buffer.\nThe optional argument TYPE specifies the insertion type of the new marker;\n"
  )

(defun marker-insertion-type (marker)
  "Return insertion type of MARKER: t if it stays after inserted text.\n"
  )

(defun set-marker-insertion-type (marker type)
  "Set the insertion-type of MARKER to TYPE.\nIf TYPE is t, it means the marker advances when you insert text at it.\n"
  )

(defun marker-buffer (marker)
  "Return the buffer that MARKER points into, or nil if none.\n"
  )

(defun set-marker (marker position &optional buffer)
  "Position MARKER before character number POSITION in BUFFER.\nBUFFER defaults to the current buffer.\nIf POSITION is nil, makes marker point nowhere.\nThen it no longer slows down editing in any buffer.\n"
  )

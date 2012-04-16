(ns emacs.filelock (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun lock-buffer (&optional file)
  "Lock FILE, if current buffer is modified.
  FILE defaults to current buffer's visited file,
  or else nothing is done if current buffer isn't visiting a file.next-read-file-uses-dialog-p is a built-in function in `C source
  code'."
  )

(defun unlock-buffer ()
  "Unlock the file visited in the current buffer.
  If the buffer is not modified, this does nothing because the file"
  )

(defun file-locked-p (filename)
  "Return a value indicating whether FILENAME is locked.
  The value is nil if the FILENAME is not locked,"
  )

(ns
 deuce.emacs.filelock
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun lock-buffer (&optional file)
  "Lock FILE, if current buffer is modified.
  FILE defaults to current buffer's visited file,
  or else nothing is done if current buffer isn't visiting a file."
  )

(defun unlock-buffer ()
  "Unlock the file visited in the current buffer.
  If the buffer is not modified, this does nothing because the file
  should not be locked in that case."
  )

(defun file-locked-p (filename)
  "Return a value indicating whether FILENAME is locked.
  The value is nil if the FILENAME is not locked,
  t if it is locked by you, else a string saying which user has locked it."
  )

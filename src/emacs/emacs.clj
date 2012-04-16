(ns emacs.emacs (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun invocation-directory ()
  )

(defun invocation-name ()
  "Return the program name that was used to run Emacs."
  )

(defun daemon-initialized ()
  "Mark the Emacs daemon as being initialized.
  This finishes the daemonization process by doing the other half of detaching"
  )

(defun daemonp ()
  "Return non-nil if the current emacs process is a daemon."
  )

(defun dump-emacs (filename symfile)
  "Dump current state of Emacs into executable file FILENAME.
  Take symbols from SYMFILE (presumably the file you executed to run Emacs).
  This is used in the file `loadup.el' when building Emacs."
  )

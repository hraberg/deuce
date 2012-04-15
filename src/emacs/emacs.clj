(ns emacs.emacs (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun invocation-directory ()
  )

(defun invocation-name ()
  "Return the program name that was used to run Emacs.\n"
  )

(defun daemon-initialized ()
  "Mark the Emacs daemon as being initialized.\nThis finishes the daemonization process by doing the other half of detaching\n"
  )

(defun daemonp ()
  "Return non-nil if the current emacs process is a daemon.\n"
  )

(defun dump-emacs (filename symfile)
  "Dump current state of Emacs into executable file FILENAME.\nTake symbols from SYMFILE (presumably the file you executed to run Emacs).\nThis is used in the file `loadup.el' when building Emacs."
  )

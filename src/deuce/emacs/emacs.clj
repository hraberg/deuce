(ns
 deuce.emacs.emacs
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun invocation-directory ()
  "Return the directory name in which the Emacs executable was located."
  )

(defun invocation-name ()
  "Return the program name that was used to run Emacs.
  Any directory names are omitted."
  )

(defun daemon-initialized ()
  "Mark the Emacs daemon as being initialized.
  This finishes the daemonization process by doing the other half of detaching
  from the parent process and its tty file descriptors."
  )

(defun daemonp ()
  "Return non-nil if the current emacs process is a daemon.
  If the daemon was given a name argument, return that name."
  )

(defun kill-emacs (&optional arg)
  "Exit the Emacs job and kill it.
  If ARG is an integer, return ARG as the exit program code.
  If ARG is a string, stuff it as keyboard input.
  
  This function is called upon receipt of the signals SIGTERM
  or SIGHUP, and upon SIGINT in batch mode.
  
  The value of `kill-emacs-hook', if not void,
  is a list of functions (of no args),
  all of which are called before Emacs is actually killed."
  )

(defun dump-emacs (filename symfile)
  "Dump current state of Emacs into executable file FILENAME.
  Take symbols from SYMFILE (presumably the file you executed to run Emacs).
  This is used in the file `loadup.el' when building Emacs.
  
  You must run Emacs in batch mode in order to dump it."
  )

(ns emacs.callproc (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun getenv-internal (variable &optional env)
  "Get the value of environment variable VARIABLE.
  VARIABLE should be a string.  Value is nil if VARIABLE is undefined in
  the environment.  Otherwise, value is a string."
  )

(defun call-process-region (start end program &optional delete buffer display &rest args)
  "Send text from START to END to a synchronous process running PROGRAM.
  The remaining arguments are optional.
  Delete the text if fourth arg DELETE is non-nil."
  )

(defun call-process (program &optional infile buffer display &rest args)
  "Call PROGRAM synchronously in separate process.
  The remaining arguments are optional.
  The program's input comes from file INFILE (nil means `/dev/null').
  Insert output in BUFFER before point; t means current buffer;
   nil for BUFFER means discard it; 0 means discard and don't wait.
  BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
  REAL-BUFFER says what to do with standard output, as above,
  while STDERR-FILE says what to do with standard error in the child.
  STDERR-FILE may be nil (discard standard error output),
  t (mix it with ordinary output), or a file name string."
  )

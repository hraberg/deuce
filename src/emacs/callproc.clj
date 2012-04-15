(ns emacs.callproc (use [deuce.core]) (:refer-clojure :only []))

(defun getenv-internal (variable &optional env)
  "Get the value of environment variable VARIABLE.\nVARIABLE should be a string.  Value is nil if VARIABLE is undefined in\nthe environment.  Otherwise, value is a string."
  )

(defun call-process-region (start end program &optional delete buffer display &rest args)
  "Send text from START to END to a synchronous process running PROGRAM.\nThe remaining arguments are optional.\nDelete the text if fourth arg DELETE is non-nil."
  )

(defun call-process (program &optional infile buffer display &rest args)
  "Call PROGRAM synchronously in separate process.\nThe remaining arguments are optional.\nThe program's input comes from file INFILE (nil means `/dev/null').\nInsert output in BUFFER before point; t means current buffer;\n nil for BUFFER means discard it; 0 means discard and don't wait.\nBUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,\nREAL-BUFFER says what to do with standard output, as above,\nwhile STDERR-FILE says what to do with standard error in the child.\nSTDERR-FILE may be nil (discard standard error output),\nt (mix it with ordinary output), or a file name string."
  )

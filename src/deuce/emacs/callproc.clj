(ns
 deuce.emacs.callproc
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun getenv-internal (variable &optional env)
  "Get the value of environment variable VARIABLE.
  VARIABLE should be a string.  Value is nil if VARIABLE is undefined in
  the environment.  Otherwise, value is a string.
  
  This function searches `process-environment' for VARIABLE.
  
  If optional parameter ENV is a list, then search this list instead of
  `process-environment', and return t when encountering a negative entry
  (an entry for a variable with no value)."
  )

(defun call-process-region (start end program &optional delete buffer display &rest args)
  "Send text from START to END to a synchronous process running PROGRAM.
  The remaining arguments are optional.
  Delete the text if fourth arg DELETE is non-nil.
  
  Insert output in BUFFER before point; t means current buffer; nil for
   BUFFER means discard it; 0 means discard and don't wait; and `(:file
   FILE)', where FILE is a file name string, means that it should be
   written to that file (if the file already exists it is overwritten).
  BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
  REAL-BUFFER says what to do with standard output, as above,
  while STDERR-FILE says what to do with standard error in the child.
  STDERR-FILE may be nil (discard standard error output),
  t (mix it with ordinary output), or a file name string.
  
  Sixth arg DISPLAY non-nil means redisplay buffer as output is inserted.
  Remaining args are passed to PROGRAM at startup as command args.
  
  If BUFFER is 0, `call-process-region' returns immediately with value nil.
  Otherwise it waits for PROGRAM to terminate
  and returns a numeric exit status or a signal description string.
  If you quit, the process is killed with SIGINT, or SIGKILL if you quit again."
  )

(defun call-process (program &optional infile buffer display &rest args)
  "Call PROGRAM synchronously in separate process.
  The remaining arguments are optional.
  The program's input comes from file INFILE (nil means `/dev/null').
  Insert output in BUFFER before point; t means current buffer; nil for BUFFER
   means discard it; 0 means discard and don't wait; and `(:file FILE)', where
   FILE is a file name string, means that it should be written to that file
   (if the file already exists it is overwritten).
  BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
  REAL-BUFFER says what to do with standard output, as above,
  while STDERR-FILE says what to do with standard error in the child.
  STDERR-FILE may be nil (discard standard error output),
  t (mix it with ordinary output), or a file name string.
  
  Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.
  Remaining arguments are strings passed as command arguments to PROGRAM.
  
  If executable PROGRAM can't be found as an executable, `call-process'
  signals a Lisp error.  `call-process' reports errors in execution of
  the program only through its return and output.
  
  If BUFFER is 0, `call-process' returns immediately with value nil.
  Otherwise it waits for PROGRAM to terminate
  and returns a numeric exit status or a signal description string.
  If you quit, the process is killed with SIGINT, or SIGKILL if you quit again."
  )

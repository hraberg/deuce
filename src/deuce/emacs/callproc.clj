(ns deuce.emacs.callproc
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c])
  (:refer-clojure :exclude []))

(defvar doc-directory nil
  "Directory containing the DOC file that comes with GNU Emacs.
  This is usually the same as `data-directory'.")

(defvar shared-game-score-directory nil
  "Directory of score files for games which come with GNU Emacs.
  If this variable is nil, then Emacs is unable to use a shared directory.")

(defvar exec-suffixes nil
  "*List of suffixes to try to find executable file names.
  Each element is a string.")

(defvar shell-file-name nil
  "*File name to load inferior shells from.
  Initialized from the SHELL environment variable, or to a system-dependent
  default if SHELL is not set.

  You can customize this variable.")

(defvar exec-path nil
  "*List of directories to search programs to run in subprocesses.
  Each element is a string (directory name) or nil (try default directory).

  You can customize this variable.")

(defvar initial-environment (map str (System/getenv))
  "List of environment variables inherited from the parent process.
  Each element should be a string of the form ENVVARNAME=VALUE.
  The elements must normally be decoded (using `locale-coding-system') for use.")

(defvar data-directory nil
  "Directory of machine-independent files that come with GNU Emacs.
  These are files intended for Emacs to use while it runs.")

(defvar process-environment nil
  "List of overridden environment variables for subprocesses to inherit.
  Each element should be a string of the form ENVVARNAME=VALUE.

  Entries in this list take precedence to those in the frame-local
  environments.  Therefore, let-binding `process-environment' is an easy
  way to temporarily change the value of an environment variable,
  irrespective of where it comes from.  To use `process-environment' to
  remove an environment variable, include only its name in the list,
  without \"=VALUE\".

  This variable is set to nil when Emacs starts.

  If multiple entries define the same variable, the first one always
  takes precedence.

  Non-ASCII characters are encoded according to the initial value of
  `locale-coding-system', i.e. the elements must normally be decoded for
  use.

  See `setenv' and `getenv'.")

(defvar exec-directory nil
  "Directory for executables for Emacs to invoke.
  More generally, this includes any architecture-dependent files
  that are built and installed from the Emacs distribution.")

(defvar configure-info-directory "/use/share/info"
  "For internal use by the build procedure only.
  This is the name of the directory in which the build procedure installed
  Emacs's info files; the default value for `Info-default-directory-list'
  includes this.")

(defun getenv-internal (variable &optional env)
  "Get the value of environment variable VARIABLE.
  VARIABLE should be a string.  Value is nil if VARIABLE is undefined in
  the environment.  Otherwise, value is a string.

  This function searches `process-environment' for VARIABLE.

  If optional parameter ENV is a list, then search this list instead of
  `process-environment', and return t when encountering a negative entry
  (an entry for a variable with no value)."
  (if-not env
    (System/getenv variable)
    (throw (IllegalArgumentException. "doesn't yet support env argument"))))

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

(ns deuce.emacs.emacs
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.fns :as fns]
            [deuce.emacs-lisp.globals :as globals])
  (:import [java.io File])
  (:refer-clojure :exclude []))

(defvar emacs-version "24.2"
  "Version numbers of this version of Emacs.")

(defvar noninteractive nil
  "Non-nil means Emacs is running without interactive terminal.")

(defvar installation-directory nil
  "A directory within which to look for the `lib-src' and `etc' directories.
  In an installed Emacs, this is normally nil.  It is non-nil if
  both `lib-src' (on MS-DOS, `info') and `etc' directories are found
  within the variable `invocation-directory' or its parent.  For example,
  this is the case when running an uninstalled Emacs executable from its
  build directory.")

(defvar invocation-directory nil
  "The directory in which the Emacs executable was found, to run it.
  The value is nil if that directory's name is not known.")

(defvar previous-system-messages-locale nil
  "Most recently used system locale for messages.")

(defvar system-type (symbol "jvm")
  "The value is a symbol indicating the type of operating system you are using.
  Special values:
    `gnu'          compiled for a GNU Hurd system.
    `gnu/linux'    compiled for a GNU/Linux system.
    `gnu/kfreebsd' compiled for a GNU system with a FreeBSD kernel.
    `darwin'       compiled for Darwin (GNU-Darwin, Mac OS X, ...).
    `ms-dos'       compiled as an MS-DOS application.
    `windows-nt'   compiled as a native W32 application.
    `cygwin'       compiled using the Cygwin library.
  Anything else (in Emacs 24.1, the possibilities are: aix, berkeley-unix,
  hpux, irix, usg-unix-v) indicates some sort of Unix system.")

(defvar inhibit-x-resources nil
  "If non-nil, X resources, Windows Registry settings, and NS defaults are not used.")

(defvar system-configuration-options nil
  "String containing the configuration options Emacs was built with.")

(defvar command-line-args (alloc/list "src/bootstrap-emacs" "-no-init-file")
  "Args passed by shell to Emacs, as a list of strings.
  Many arguments are deleted from the list as they are processed.")

(defvar invocation-name nil
  "The program name that was used to run Emacs.
  Any directory names are omitted.")

(defvar system-messages-locale nil
  "System locale for messages.")

(defvar dynamic-library-alist nil
  "Alist of dynamic libraries vs external files implementing them.
  Each element is a list (LIBRARY FILE...), where the car is a symbol
  representing a supported external library, and the rest are strings giving
  alternate filenames for that library.

  Emacs tries to load the library from the files in the order they appear on
  the list; if none is loaded, the running session of Emacs won't have access
  to that library.

  Note that image types `pbm' and `xbm' do not need entries in this variable
  because they do not depend on external libraries and are always available.

  Also note that this is not a generic facility for accessing external
  libraries; only those already known by Emacs will be loaded.")

(fns/put 'dynamic-library-alist 'risky-local-variable true)

(defvar emacs-copyright "Copyright (C) 2012 Free Software Foundation, Inc."
  "Short copyright string for this version of Emacs.")

(defvar system-time-locale nil
  "System locale for time.")

(defvar path-separator File/pathSeparator
  "String containing the character that separates directories in
  search paths, such as PATH and other similar environment variables.")

(defvar system-configuration (format "jvm-%s_clojure-%s"
                                     (System/getProperty "java.version")
                                     (clojure-version))
  "Value is string indicating configuration Emacs was built for.
  On MS-Windows, the value reflects the OS flavor and version on which
  Emacs is running.")

(defvar kill-emacs-hook nil
  "Hook to be run when `kill-emacs' is called.
  Since `kill-emacs' may be invoked when the terminal is disconnected (or
  in other similar situations), functions placed on this hook should not
  expect to be able to interact with the user.  To ask for confirmation,
  see `kill-emacs-query-functions' instead.

  Before Emacs 24.1, the hook was not run in batch mode, i.e., if
  `noninteractive' was non-nil.")

(defvar before-init-time nil
  "Value of `current-time' before Emacs begins initialization.")

(defvar after-init-time nil
  "Value of `current-time' after loading the init files.
  This is nil during initialization.")

(defvar previous-system-time-locale nil
  "Most recently used system locale for time.")

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
  (doall (map #(%) globals/kill-emacs-hook))
  (System/exit (if (integer? arg) arg 0)))

(defun dump-emacs (filename symfile)
  "Dump current state of Emacs into executable file FILENAME.
  Take symbols from SYMFILE (presumably the file you executed to run Emacs).
  This is used in the file `loadup.el' when building Emacs.

  You must run Emacs in batch mode in order to dump it."
  )

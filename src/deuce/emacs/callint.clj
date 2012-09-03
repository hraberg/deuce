(ns
 deuce.emacs.callint
 (use [deuce.emacs-lisp :only (defun defvar)])
 (require [clojure.core :as c])
 (:refer-clojure :exclude []))

(defvar prefix-arg nil
  "The value of the prefix argument for the next editing command.
  It may be a number, or the symbol `-' for just a minus sign as arg,
  or a list whose car is a number for just one or more C-u's
  or nil if no argument has been specified.
  
  You cannot examine this variable to find the argument for this command
  since it has been set to nil by the time you can look.
  Instead, you should use the variable `current-prefix-arg', although
  normally commands can get this prefix argument with (interactive \"P\").")

(defvar command-history nil
  "List of recent commands that read arguments from terminal.
  Each command is represented as a form to evaluate.
  
  Maximum length of the history list is determined by the value
  of `history-length', which see.")

(defvar last-prefix-arg nil
  "The value of the prefix argument for the previous editing command.
  See `prefix-arg' for the meaning of the value.")

(defvar command-debug-status nil
  "Debugging status of current interactive command.
  Bound each time `call-interactively' is called;
  may be set by the debugger as a reminder for itself.")

(defvar current-prefix-arg nil
  "The value of the prefix argument for this editing command.
  It may be a number, or the symbol `-' for just a minus sign as arg,
  or a list whose car is a number for just one or more C-u's
  or nil if no argument has been specified.
  This is what `(interactive \"P\")' returns.")

(defvar mark-even-if-inactive nil
  "*Non-nil means you can use the mark even when inactive.
  This option makes a difference in Transient Mark mode.
  When the option is non-nil, deactivation of the mark
  turns off region highlighting, but commands that use the mark
  behave as if the mark were still active.
  
  You can customize this variable.")

(defvar mouse-leave-buffer-hook nil
  "Hook to run when about to switch windows with a mouse command.
  Its purpose is to give temporary modes such as Isearch mode
  a way to turn themselves off when a mouse command switches windows.")

(defun call-interactively (function &optional record-flag keys)
  "Call FUNCTION, providing args according to its interactive calling specs.
  Return the value FUNCTION returns.
  The function contains a specification of how to do the argument reading.
  In the case of user-defined functions, this is specified by placing a call
  to the function `interactive' at the top level of the function body.
  See `interactive'.
  
  Optional second arg RECORD-FLAG non-nil
  means unconditionally put this command in the command-history.
  Otherwise, this is done only if an arg is read using the minibuffer.
  
  Optional third arg KEYS, if given, specifies the sequence of events to
  supply, as a vector, if the command inquires which events were used to
  invoke it.  If KEYS is omitted or nil, the return value of
  `this-command-keys-vector' is used."
  )

(defun prefix-numeric-value (raw)
  "Return numeric meaning of raw prefix argument RAW.
  A raw prefix argument is what you get from `(interactive \"P\")'.
  Its numeric meaning is what you would get from `(interactive \"p\")'."
  )

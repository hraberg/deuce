(ns
 deuce.emacs.callint
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

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

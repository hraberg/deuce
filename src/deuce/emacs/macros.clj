(ns
 deuce.emacs.macros
 (use [deuce.emacs-lisp :only (defun defvar)])
 (require [clojure.core :as c])
 (:refer-clojure :exclude []))

(defvar last-kbd-macro nil
  "Last kbd macro defined, as a string or vector; nil if none defined.")

(defvar defining-kbd-macro nil
  "Non-nil while a keyboard macro is being defined.  Don't set this!
  The value is the symbol `append' while appending to the definition of
  an existing macro.")

(defvar kbd-macro-termination-hook nil
  "Normal hook run whenever a keyboard macro terminates.
  This is run whether the macro ends normally or prematurely due to an error.")

(defvar executing-kbd-macro-index nil
  "Index in currently executing keyboard macro; undefined if none executing.")

(defvar executing-kbd-macro nil
  "Currently executing keyboard macro (string or vector).
  This is nil when not executing a keyboard macro.")

(defun start-kbd-macro (append &optional no-exec)
  "Record subsequent keyboard input, defining a keyboard macro.
  The commands are recorded even as they are executed.
  Use M-x end-kbd-macro to finish recording and make the macro available.
  Use M-x name-last-kbd-macro to give it a permanent name.
  Non-nil arg (prefix arg) means append to last macro defined;
  this begins by re-executing that macro as if you typed it again.
  If optional second arg, NO-EXEC, is non-nil, do not re-execute last
  macro before appending to it."
  )

(defun defining-kbd-macro (append &optional no-exec)
  "Record subsequent keyboard input, defining a keyboard macro.
  The commands are recorded even as they are executed.
  Use M-x end-kbd-macro to finish recording and make the macro available.
  Use M-x name-last-kbd-macro to give it a permanent name.
  Non-nil arg (prefix arg) means append to last macro defined;
  this begins by re-executing that macro as if you typed it again.
  If optional second arg, NO-EXEC, is non-nil, do not re-execute last
  macro before appending to it."
  )

(defun execute-kbd-macro (macro &optional count loopfunc)
  "Execute MACRO as string of editor command characters.
  If MACRO is a symbol, its function definition is used.
  COUNT is a repeat count, or nil for once, or 0 for infinite loop.
  
  Optional third arg LOOPFUNC may be a function that is called prior to
  each iteration of the macro.  Iteration stops if LOOPFUNC returns nil."
  )

(defun end-kbd-macro (&optional repeat loopfunc)
  "Finish defining a keyboard macro.
  The definition was started by M-x start-kbd-macro.
  The macro is now available for use via M-x call-last-kbd-macro,
  or it can be given a name with M-x name-last-kbd-macro and then invoked
  under that name.
  
  With numeric arg, repeat macro now that many times,
  counting the definition just completed as the first repetition.
  An argument of zero means repeat until error.
  
  In Lisp, optional second arg LOOPFUNC may be a function that is called prior to
  each iteration of the macro.  Iteration stops if LOOPFUNC returns nil."
  )

(defun call-last-kbd-macro (&optional prefix loopfunc)
  "Call the last keyboard macro that you defined with M-x start-kbd-macro.
  
  A prefix argument serves as a repeat count.  Zero means repeat until error.
  
  To make a macro permanent so you can call it even after
  defining others, use M-x name-last-kbd-macro.
  
  In Lisp, optional second arg LOOPFUNC may be a function that is called prior to
  each iteration of the macro.  Iteration stops if LOOPFUNC returns nil."
  )

(defun cancel-kbd-macro-events ()
  "Cancel the events added to a keyboard macro for this command."
  )

(defun store-kbd-macro-event (event)
  "Store EVENT into the keyboard macro being defined."
  )

(ns
 emacs.macros
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

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
  COUNT is a repeat count, or nil for once, or 0 for infinite loop."
  )

(defun end-kbd-macro (&optional repeat loopfunc)
  "Finish defining a keyboard macro.
  The definition was started by M-x start-kbd-macro.
  The macro is now available for use via M-x call-last-kbd-macro,
  or it can be given a name with M-x name-last-kbd-macro and then invoked
  under that name."
  )

(defun call-last-kbd-macro (&optional prefix loopfunc)
  "Call the last keyboard macro that you defined with M-x start-kbd-macro."
  )

(defun cancel-kbd-macro-events ()
  "Cancel the events added to a keyboard macro for this command."
  )

(defun store-kbd-macro-event (event)
  "Store EVENT into the keyboard macro being defined."
  )

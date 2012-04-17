(ns emacs.eval (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun user-variable-p (variable)
  "Return t if VARIABLE is intended to be set and modified by users.
  (The alternative is a variable used internally in a Lisp program.)
  A variable is a user variable if
  (1) the first character of its documentation is `*', or
  (2) it is customizable (its property list contains a non-nil value
      of `standard-value' or `custom-autoload'), or
  (3) it is an alias for another user variable.
  Return nil if VARIABLE is an alias and there is a loop in the
  chain of symbols."
  )

(defun autoload (function file &optional docstring interactive type)
  "Define FUNCTION to autoload from FILE.
  FUNCTION is a symbol; FILE is a file name string to pass to `load'.
  Third arg DOCSTRING is documentation for the function.
  Fourth arg INTERACTIVE if non-nil says function can be called interactively.
  Fifth arg TYPE indicates the type of the object:
     nil or omitted says FUNCTION is a function,
     `keymap' says FUNCTION is really a keymap, and
     `macro' or t says FUNCTION is really a macro.
  Third through fifth args give info about the real definition.
  They default to nil.
  If FUNCTION is already defined other than as an autoload,
  this does nothing and returns nil."
  )

(defun fetch-bytecode (object)
  "If byte-compiled OBJECT is lazy-loaded, fetch it now."
  )

(defun signal (error-symbol data)
  "Signal an error.  Args are ERROR-SYMBOL and associated DATA.
  This function does not return."
  )

(defun called-interactively-p (kind)
  "Return t if the containing function was called by `call-interactively'.
  If KIND is `interactive', then only return t if the call was made
  interactively by the user, i.e. not in `noninteractive' mode nor
  when `executing-kbd-macro'.
  If KIND is `any', on the other hand, it will return t for any kind of
  interactive call, including being called as the binding of a key, or
  from a keyboard macro, or in `noninteractive' mode."
  )

(defun run-hook-with-args (hook &rest args)
  "Run HOOK with the specified arguments ARGS.
  HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
  value, that value may be a function or a list of functions to be
  called to run the hook.  If the value is a function, it is called with
  the given arguments and its return value is returned.  If it is a list
  of functions, those functions are called, in order,
  with the given arguments ARGS.
  It is best not to depend on the value returned by `run-hook-with-args',
  as that may change."
  )

(defun funcall (function &rest arguments)
  "Call first argument as a function, passing remaining arguments to it.
  Return the value that function returns.
  Thus, (funcall 'cons 'x 'y) returns (x . y)."
  )

(defun interactive-p ()
  "This function is obsolete since 23.2;
  use `called-interactively-p' instead."
  )

(defun defvaralias (new-alias base-variable &optional docstring)
  "Make NEW-ALIAS a variable alias for symbol BASE-VARIABLE.
  Aliased variables always have the same value; setting one sets the other.
  Third arg DOCSTRING, if non-nil, is documentation for NEW-ALIAS.  If it is
  omitted or nil, NEW-ALIAS gets the documentation string of BASE-VARIABLE,
  or of the variable at the end of the chain of aliases, if BASE-VARIABLE is
  itself an alias.  If NEW-ALIAS is bound, and BASE-VARIABLE is not,
  then the value of BASE-VARIABLE is set to that of NEW-ALIAS.
  The return value is BASE-VARIABLE."
  )

(defun run-hook-with-args-until-success (hook &rest args)
  "Run HOOK with the specified arguments ARGS.
  HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
  value, that value may be a function or a list of functions to be
  called to run the hook.  If the value is a function, it is called with
  the given arguments and its return value is returned.
  If it is a list of functions, those functions are called, in order,
  with the given arguments ARGS, until one of them
  returns a non-nil value.  Then we return that value.
  However, if they all return nil, we return nil."
  )

(defun throw (tag value)
  "Throw to the catch for TAG and return VALUE from it.
  Both TAG and VALUE are evalled."
  )

(defun backtrace ()
  "Print a trace of Lisp function calls currently active.
  Output stream used is value of `standard-output'."
  )

(defun run-hook-with-args-until-failure (hook &rest args)
  "Run HOOK with the specified arguments ARGS.
  HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
  value, that value may be a function or a list of functions to be
  called to run the hook.  If the value is a function, it is called with
  the given arguments and its return value is returned.
  If it is a list of functions, those functions are called, in order,
  with the given arguments ARGS, until one of them returns nil.
  Then we return nil.  However, if they all return non-nil, we return non-nil."
  )

(defun apply (function &rest arguments)
  "Call FUNCTION with our remaining args, using our last arg as list of args.
  Then return the value FUNCTION returns.
  Thus, (apply '+ 1 2 '(3 4)) returns 10."
  )

(defun run-hooks (&rest hooks)
  "Run each hook in HOOKS.
  Each argument should be a symbol, a hook variable.
  These symbols are processed in the order specified.
  If a hook symbol has a non-nil value, that value may be a function
  or a list of functions to be called to run the hook.
  If the value is a function, it is called with no arguments.
  If it is a list, the elements are called, in order, with no arguments."
  )

(defun eval (form)
  "Evaluate FORM and return its value."
  )

(defun backtrace-debug (level flag)
  "Set the debug-on-exit flag of eval frame LEVEL levels down to FLAG.
  The debugger is entered when that frame exits, if the flag is non-nil."
  )

(defun backtrace-frame (nframes)
  "Return the function and arguments NFRAMES up from current execution point.
  If that frame has not evaluated the arguments yet (or is a special form),
  the value is (nil FUNCTION ARG-FORMS...).
  If that frame has evaluated its arguments and called its function already,
  the value is (t FUNCTION ARG-VALUES...).
  A &rest arg is represented as the tail of the list ARG-VALUES.
  FUNCTION is whatever was supplied as car of evaluated list,
  or a lambda expression for macro calls.
  If NFRAMES is more than the number of frames, the value is nil."
  )

(defun commandp (function &optional for-call-interactively)
  "Non-nil if FUNCTION makes provisions for interactive calling.
  This means it contains a description for how to read arguments to give it.
  The value is nil for an invalid function or a symbol with no function
  definition."
  )

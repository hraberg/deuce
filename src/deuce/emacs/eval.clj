(ns
 deuce.emacs.eval
 (use [deuce.emacs-lisp :only (defun defvar)])
 (require [clojure.core :as c]
          [deuce.emacs.data :as data]
          [deuce.emacs-lisp :as el])
 (:refer-clojure :exclude [apply eval]))

(defvar debugger nil
  "Function to call to invoke debugger.
  If due to frame exit, args are `exit' and the value being returned;
   this function's value will be returned instead of that.
  If due to error, args are `error' and a list of the args to `signal'.
  If due to `apply' or `funcall' entry, one arg, `lambda'.
  If due to `eval' entry, one arg, t.")

(defvar inhibit-quit nil
  "Non-nil inhibits C-g quitting from happening immediately.
  Note that `quit-flag' will still be set by typing C-g,
  so a quit will be signaled as soon as `inhibit-quit' is nil.
  To prevent this happening, set `quit-flag' to nil
  before making `inhibit-quit' nil.")

(defvar max-lisp-eval-depth nil
  "*Limit on depth in `eval', `apply' and `funcall' before error.

  This limit serves to catch infinite recursions for you before they cause
  actual stack overflow in C, which would be fatal for Emacs.
  You can safely make it considerably larger than its default value,
  if that proves inconveniently small.  However, if you increase it too far,
  Emacs could overflow the real C stack, and crash.

  You can customize this variable.")

(defvar macro-declaration-function nil
  "Function to process declarations in a macro definition.
  The function will be called with two args MACRO and DECL.
  MACRO is the name of the macro being defined.
  DECL is a list `(declare ...)' containing the declarations.
  The value the function returns is not used.")

(defvar quit-flag nil
  "Non-nil causes `eval' to abort, unless `inhibit-quit' is non-nil.
  If the value is t, that means do an ordinary quit.
  If the value equals `throw-on-input', that means quit by throwing
  to the tag specified in `throw-on-input'; it's for handling `while-no-input'.
  Typing C-g sets `quit-flag' to t, regardless of `inhibit-quit',
  but `inhibit-quit' non-nil prevents anything from taking notice of that.")

(defvar debug-on-signal nil
  "*Non-nil means call the debugger regardless of condition handlers.
  Note that `debug-on-error', `debug-on-quit' and friends
  still determine whether to handle the particular condition.")

(defvar signal-hook-function nil
  "If non-nil, this is a function for `signal' to call.
  It receives the same arguments that `signal' was given.
  The Edebug package uses this to regain control.")

(defvar debug-on-next-call nil
  "Non-nil means enter debugger before next `eval', `apply' or `funcall'.")

(defvar debug-ignored-errors nil
  "*List of errors for which the debugger should not be called.
  Each element may be a condition-name or a regexp that matches error messages.
  If any element applies to a given error, that error skips the debugger
  and just returns to top level.
  This overrides the variable `debug-on-error'.
  It does not apply to errors handled by `condition-case'.

  You can customize this variable.")

(defvar debug-on-quit nil
  "*Non-nil means enter debugger if quit is signaled (C-g, for example).
  Does not apply if quit is handled by a `condition-case'.

  You can customize this variable.")

(defvar debug-on-error nil
  "*Non-nil means enter debugger if an error is signaled.
  Does not apply to errors handled by `condition-case' or those
  matched by `debug-ignored-errors'.
  If the value is a list, an error only means to enter the debugger
  if one of its condition symbols appears in the list.
  When you evaluate an expression interactively, this variable
  is temporarily non-nil if `eval-expression-debug-on-error' is non-nil.
  The command `toggle-debug-on-error' toggles this.
  See also the variable `debug-on-quit'.

  You can customize this variable.")

(defvar debugger-may-continue nil
  "Non-nil means debugger may continue execution.
  This is nil when the debugger is called under circumstances where it
  might not be safe to continue.")

(defvar max-specpdl-size nil
  "*Limit on number of Lisp variable bindings and `unwind-protect's.
  If Lisp code tries to increase the total number past this amount,
  an error is signaled.
  You can safely use a value considerably larger than the default value,
  if that proves inconveniently small.  However, if you increase it too far,
  Emacs could run out of memory trying to make the stack bigger.

  You can customize this variable.")

(defun user-variable-p (variable)
  "Return t if VARIABLE is intended to be set and modified by users.
  (The alternative is a variable used internally in a Lisp program.)

  This function returns t if (i) the first character of its
  documentation is `*', or (ii) it is customizable (its property list
  contains a non-nil value of `standard-value' or `custom-autoload'), or
  (iii) it is an alias for a user variable.

  But condition (i) is considered obsolete, so for most purposes this is
  equivalent to `custom-variable-p'."
  )

(defun special-variable-p (symbol)
  "Return non-nil if SYMBOL's global binding has been declared special.
  A special variable is one that will be bound dynamically, even in a
  context where binding is lexical by default."
  )

(defun functionp (object)
  "Non-nil if OBJECT is a function."
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
  This function does not return.

  An error symbol is a symbol with an `error-conditions' property
  that is a list of condition names.
  A handler for any of those names will get to handle this signal.
  The symbol `error' should normally be one of them.

  DATA should be a list.  Its elements are printed as part of the error message.
  See Info anchor `(elisp)Definition of signal' for some details on how this
  error message is constructed.
  If the signal is handled, DATA is made available to the handler.
  See also the function `condition-case'."
  (el/throw error-symbol data))

(defun called-interactively-p (kind)
  "Return t if the containing function was called by `call-interactively'.
  If KIND is `interactive', then only return t if the call was made
  interactively by the user, i.e. not in `noninteractive' mode nor
  when `executing-kbd-macro'.
  If KIND is `any', on the other hand, it will return t for any kind of
  interactive call, including being called as the binding of a key, or
  from a keyboard macro, or in `noninteractive' mode.

  The only known proper use of `interactive' for KIND is in deciding
  whether to display a helpful message, or how to display it.  If you're
  thinking of using it for any other purpose, it is quite likely that
  you're making a mistake.  Think: what do you want to do when the
  command is called from a keyboard macro?

  Instead of using this function, it is sometimes cleaner to give your
  function an extra optional argument whose `interactive' spec specifies
  non-nil unconditionally (\"p\" is a good way to do this), or via
  (not (or executing-kbd-macro noninteractive))."
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
  as that may change.

  Do not use `make-local-variable' to make a hook variable buffer-local.
  Instead, use `add-hook' and specify t for the LOCAL argument."
  (let [hook (data/symbol-value hook)]
    (dorun (map #(c/apply % args) (if (fn? hook) [hook] hook)))))

(defun funcall (function &rest arguments)
  "Call first argument as a function, passing remaining arguments to it.
  Return the value that function returns.
  Thus, (funcall 'cons 'x 'y) returns (x . y)."
  (c/apply (if (symbol? function) (data/symbol-function function) function) arguments))

(defun run-hook-wrapped (hook wrap-function &rest args)
  "Run HOOK, passing each function through WRAP-FUNCTION.
  I.e. instead of calling each function FUN directly with arguments ARGS,
  it calls WRAP-FUNCTION with arguments FUN and ARGS.
  As soon as a call to WRAP-FUNCTION returns non-nil, `run-hook-wrapped'
  aborts and returns that value."
  )

(defun interactive-p ()
  "This function is obsolete since 23.2;
  use `called-interactively-p' instead.

  Return t if the containing function was run directly by user input.
  This means that the function was called with `call-interactively'
  (which includes being called as the binding of a key)
  and input is currently coming from the keyboard (not a keyboard macro),
  and Emacs is not running in batch mode (`noninteractive' is nil).

  The only known proper use of `interactive-p' is in deciding whether to
  display a helpful message, or how to display it.  If you're thinking
  of using it for any other purpose, it is quite likely that you're
  making a mistake.  Think: what do you want to do when the command is
  called from a keyboard macro?

  To test whether your function was called with `call-interactively',
  either (i) add an extra optional argument and give it an `interactive'
  spec that specifies non-nil unconditionally (such as \"p\"); or (ii)
  use `called-interactively-p'."
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
  (if-let [base (ns-resolve 'deuce.emacs-lisp.globals base-variable)]
    (el/defvar-helper* 'deuce.emacs-lisp.globals new-alias
      @base (or docstring (-> base meta :doc)))
    (when-let [new (ns-resolve 'deuce.emacs-lisp.globals new-alias)]
      (el/defvar-helper* 'deuce.emacs-lisp.globals base-variable
        @new (or docstring (-> new meta :doc)))))
  base-variable)

(defun run-hook-with-args-until-success (hook &rest args)
  "Run HOOK with the specified arguments ARGS.
  HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
  value, that value may be a function or a list of functions to be
  called to run the hook.  If the value is a function, it is called with
  the given arguments and its return value is returned.
  If it is a list of functions, those functions are called, in order,
  with the given arguments ARGS, until one of them
  returns a non-nil value.  Then we return that value.
  However, if they all return nil, we return nil.

  Do not use `make-local-variable' to make a hook variable buffer-local.
  Instead, use `add-hook' and specify t for the LOCAL argument."
  )

(defun throw (tag value)
  "Throw to the catch for TAG and return VALUE from it.
  Both TAG and VALUE are evalled."
  (el/throw tag value))

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
  Then we return nil.  However, if they all return non-nil, we return non-nil.

  Do not use `make-local-variable' to make a hook variable buffer-local.
  Instead, use `add-hook' and specify t for the LOCAL argument."
  )

(defun apply (function &rest arguments)
  "Call FUNCTION with our remaining args, using our last arg as list of args.
  Then return the value FUNCTION returns.
  Thus, (apply '+ 1 2 '(3 4)) returns 10."
  (c/apply (if (symbol? function) (data/symbol-function function) function)
           (concat (butlast arguments) (last arguments))))

(defun run-hooks (&rest hooks)
  "Run each hook in HOOKS.
  Each argument should be a symbol, a hook variable.
  These symbols are processed in the order specified.
  If a hook symbol has a non-nil value, that value may be a function
  or a list of functions to be called to run the hook.
  If the value is a function, it is called with no arguments.
  If it is a list, the elements are called, in order, with no arguments.

  Major modes should not use this function directly to run their mode
  hook; they should use `run-mode-hooks' instead.

  Do not use `make-local-variable' to make a hook variable buffer-local.
  Instead, use `add-hook' and specify t for the LOCAL argument."
  (doseq [hook hooks]
    (run-hook-with-args hook)))

(defun eval (form &optional lexical)
  "Evaluate FORM and return its value.
  If LEXICAL is t, evaluate using lexical scoping."
  (el/eval form lexical))

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
  definition.

  Interactively callable functions include strings and vectors (treated
  as keyboard macros), lambda-expressions that contain a top-level call
  to `interactive', autoload definitions made by `autoload' with non-nil
  fourth argument, and some of the built-in functions of Lisp.

  Also, a symbol satisfies `commandp' if its function definition does so.

  If the optional argument FOR-CALL-INTERACTIVELY is non-nil,
  then strings and vectors are not accepted."
  )
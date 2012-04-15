(ns emacs.eval (use [deuce.core]) (:refer-clojure :only []))

(defun user-variable-p (variable)
  "Return t if VARIABLE is intended to be set and modified by users.\n(The alternative is a variable used internally in a Lisp program.)\nA variable is a user variable if\n(1) the first character of its documentation is `*', or\n(2) it is customizable (its property list contains a non-nil value\n    of `standard-value' or `custom-autoload'), or\n(3) it is an alias for another user variable.\nReturn nil if VARIABLE is an alias and there is a loop in the\n"
  )

(defun autoload (function file &optional docstring interactive type)
  "Define FUNCTION to autoload from FILE.\nFUNCTION is a symbol; FILE is a file name string to pass to `load'.\nThird arg DOCSTRING is documentation for the function.\nFourth arg INTERACTIVE if non-nil says function can be called interactively.\nFifth arg TYPE indicates the type of the object:\n   nil or omitted says FUNCTION is a function,\n   `keymap' says FUNCTION is really a keymap, and\n   `macro' or t says FUNCTION is really a macro.\nThird through fifth args give info about the real definition.\nThey default to nil.\nIf FUNCTION is already defined other than as an autoload,\nthis does nothing and returns nil.save-current-buffer is a special form in `C source code'."
  )

(defun fetch-bytecode (object)
  )

(defun signal (error-symbol data)
  "Signal an error.  Args are ERROR-SYMBOL and associated DATA.\nThis function does not return."
  )

(defun called-interactively-p (kind)
  "Return t if the containing function was called by `call-interactively'.\nIf KIND is `interactive', then only return t if the call was made\ninteractively by the user, i.e. not in `noninteractive' mode nor\nwhen `executing-kbd-macro'.\nIf KIND is `any', on the other hand, it will return t for any kind of\ninteractive call, including being called as the binding of a key, or\nfrom a keyboard macro, or in `noninteractive' mode."
  )

(defun run-hook-with-args (hook &rest args)
  "Run HOOK with the specified arguments ARGS.\nHOOK should be a symbol, a hook variable.  If HOOK has a non-nil\nvalue, that value may be a function or a list of functions to be\ncalled to run the hook.  If the value is a function, it is called with\nthe given arguments and its return value is returned.  If it is a list\nof functions, those functions are called, in order,\nwith the given arguments ARGS.\nIt is best not to depend on the value returned by `run-hook-with-args',\nas that may change."
  )

(defun funcall (function &rest arguments)
  "Call first argument as a function, passing remaining arguments to it.\nReturn the value that function returns.\n"
  )

(defun interactive-p ()
  "This function is obsolete since 23.2;\nuse `called-interactively-p' instead."
  )

(defun defvaralias (new-alias base-variable &optional docstring)
  "Make NEW-ALIAS a variable alias for symbol BASE-VARIABLE.\nAliased variables always have the same value; setting one sets the other.\nThird arg DOCSTRING, if non-nil, is documentation for NEW-ALIAS.  If it is\nomitted or nil, NEW-ALIAS gets the documentation string of BASE-VARIABLE,\nor of the variable at the end of the chain of aliases, if BASE-VARIABLE is\nitself an alias.  If NEW-ALIAS is bound, and BASE-VARIABLE is not,\nthen the value of BASE-VARIABLE is set to that of NEW-ALIAS.\n"
  )

(defun throw (tag value)
  "Throw to the catch for TAG and return VALUE from it.\nBoth TAG and VALUE are evalled.unhandled-file-name-directory is a built-in function in `C source\ncode'."
  )

(defun apply (function &rest arguments)
  "Call FUNCTION with our remaining args, using our last arg as list of args.\nThen return the value FUNCTION returns.\n"
  )

(defun run-hooks (&rest hooks)
  "Run each hook in HOOKS.\nEach argument should be a symbol, a hook variable.\nThese symbols are processed in the order specified.\nIf a hook symbol has a non-nil value, that value may be a function\nor a list of functions to be called to run the hook.\nIf the value is a function, it is called with no arguments.\nIf it is a list, the elements are called, in order, with no arguments."
  )

(defun eval (form)
  )

(defun backtrace-debug (level flag)
  "Set the debug-on-exit flag of eval frame LEVEL levels down to FLAG.\n"
  )

(defun backtrace-frame (nframes)
  "Return the function and arguments NFRAMES up from current execution point.\nIf that frame has not evaluated the arguments yet (or is a special form),\nthe value is (nil FUNCTION ARG-FORMS...).\nIf that frame has evaluated its arguments and called its function already,\nthe value is (t FUNCTION ARG-VALUES...).\nA &rest arg is represented as the tail of the list ARG-VALUES.\nFUNCTION is whatever was supplied as car of evaluated list,\nor a lambda expression for macro calls.\n"
  )

(defun commandp (function &optional for-call-interactively)
  "Non-nil if FUNCTION makes provisions for interactive calling.\nThis means it contains a description for how to read arguments to give it.\nThe value is nil for an invalid function or a symbol with no function\ndefinition."
  )

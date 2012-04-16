(ns emacs.data (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun natnump (object)
  )

(defun markerp (object)
  )

(defun ash (value count)
  "Return VALUE with its bits shifted left by COUNT.
  If COUNT is negative, shifting is actually to the right."
  )

(defun type-of (object)
  "Return a symbol representing the type of OBJECT.
  The symbol returned names the object's basic type;"
  )

(defun indirect-function (object &optional noerror)
  "Return the function at the end of OBJECT's function chain.
  If OBJECT is not a symbol, just return it.  Otherwise, follow all
  function indirections to find the final function binding and return it.
  If the final symbol in the chain is unbound, signal a void-function error.
  Optional arg NOERROR non-nil means to return nil instead of signalling.
  Signal a cyclic-function-indirection error if there is a loop in the"
  )

(defun symbol-name (symbol)
  )

(defun makunbound (symbol)
  "Make SYMBOL's value be void."
  )

(defun interactive-form (cmd)
  "Return the interactive form of CMD or nil if none.
  If CMD is not a command, the return value is nil."
  )

(defun logior (&rest ints-or-markers)
  "Return bitwise-or of all the arguments."
  )

(defun sequencep (object)
  )

(defun zerop (number)
  "Return t if NUMBER is zero.other-window is an interactive built-in function in `C source code'."
  )

(defun indirect-variable (object)
  "Return the variable at the end of OBJECT's variable chain.
  If OBJECT is a symbol, follow all variable indirections and return the final
  variable.  If OBJECT is not a symbol, just return it.
  Signal a cyclic-variable-indirection error if there is a loop in the"
  )

(defun symbol-value (symbol)
  )

(defun keywordp (object)
  "Return t if OBJECT is a keyword.
  This means that it is a symbol with a print name beginning with `:'"
  )

(defun (core/symbol "1+") (number)
  "Return NUMBER plus one.  NUMBER may be a number or a marker.
  Markers are converted to integers.abort-recursive-edit is an interactive built-in function in `C source
  code'."
  )

(defun subrp (object)
  )

(defun symbol-plist (symbol)
  )

(defun stringp (object)
  "Return t if OBJECT is a string.function is a special form in `C source code'."
  )

(defun integerp (object)
  )

(defun fboundp (symbol)
  )

(defun % (x y)
  "Return remainder of X divided by Y."
  )

(defun + (&rest numbers-or-markers)
  )

(defun lsh (value count)
  "Return VALUE with its bits shifted left by COUNT.
  If COUNT is negative, shifting is actually to the right."
  )

(defun eq (obj1 obj2)
  )

(defun * (&rest numbers-or-markers)
  )

(defun - (&optional number-or-marker &rest more-numbers-or-markers)
  "Negate number or subtract numbers or markers and return the result.
  With one arg, negates it.  With more than one arg,"
  )

(defun multibyte-string-p (object)
  )

(defun logxor (&rest ints-or-markers)
  "Return bitwise-exclusive-or of all the arguments."
  )

(defun floatp (object)
  )

(defun number-or-marker-p (object)
  )

(defun cdr-safe (object)
  )

(defun / (dividend divisor &rest divisors)
  "Return first argument divided by all the remaining arguments."
  )

(defun byteorder ()
  "Return the byteorder for the machine.
  Returns 66 (ASCII uppercase B) for big endian machines or 108 (ASCII"
  )

(defun subr-name (subr)
  "Return name of subroutine SUBR."
  )

(defun numberp (object)
  )

(defun logand (&rest ints-or-markers)
  "Return bitwise-and of all the arguments."
  )

(defun consp (object)
  )

(defun listp (object)
  "Return t if OBJECT is a list, that is, a cons cell or nil."
  )

(defun aref (array idx)
  "Return the element of ARRAY at index IDX.
  ARRAY may be a vector, a string, a char-table, a bool-vector,"
  )

(defun wholenump (object)
  )

(defun aset (array idx newelt)
  "Store into the element of ARRAY at index IDX the value NEWELT.
  Return NEWELT.  ARRAY may be a vector, a string, a char-table or a"
  )

(defun arrayp (object)
  )

(defun vectorp (object)
  )

(defun fmakunbound (symbol)
  "Make SYMBOL's function definition be void."
  )

(defun lognot (number)
  "Return the bitwise complement of NUMBER.  NUMBER must be an integer.move-to-window-line is an interactive built-in function in `C source
  code'."
  )

(defun setcdr (cell newcdr)
  "Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.setq is a special form in `C source code'."
  )

(defun set (symbol newval)
  "Set SYMBOL's value to NEWVAL, and return NEWVAL.narrow-to-region is an interactive built-in function in `C source
  code'."
  )

(defun < (num1 num2)
  )

(defun car-safe (object)
  )

(defun fset (symbol definition)
  "Set SYMBOL's function definition to DEFINITION, and return DEFINITION.upcase-word is an interactive built-in function in `C source code'."
  )

(defun cdr (list)
  "Return the cdr of LIST.  If arg is nil, return nil.
  Error if arg is not nil and not a cons cell.  See also `cdr-safe'."
  )

(defun (core/symbol "slash-equals") (num1 num2)
  )

(defun = (num1 num2)
  )

(defun char-or-string-p (object)
  )

(defun vector-or-char-table-p (object)
  )

(defun bufferp (object)
  )

(defun > (num1 num2)
  )

(defun max (number-or-marker &rest numbers-or-markers)
  "Return largest of all the arguments (which must be numbers or markers)."
  )

(defun local-variable-if-set-p (variable &optional buffer)
  "Non-nil if VARIABLE will be local in buffer BUFFER when set there.
  More precisely, this means that setting the variable (with `set' or`setq'),
  while it does not have a `let'-style binding that was made in BUFFER,
  will produce a buffer local binding.  See Info node
  `(elisp)Creating Buffer-Local'.
  BUFFER defaults to the current buffer.make-variable-frame-local is an interactive built-in function in `C
  source code'."
  )

(defun default-boundp (symbol)
  "Return t if SYMBOL has a non-void default value.
  This is the value that is seen in buffers that do not have their own values"
  )

(defun nlistp (object)
  )

(defun >= (num1 num2)
  "Return t if first arg is greater than or equal to second arg."
  )

(defun boundp (symbol)
  )

(defun default-value (symbol)
  "Return SYMBOL's default value.
  This is the value that is seen in buffers that do not have their own values
  for this variable.  The default value is meaningful for variables with"
  )

(defun setcar (cell newcar)
  )

(defun symbolp (object)
  )

(defun <= (num1 num2)
  "Return t if first arg is less than or equal to second arg."
  )

(defun local-variable-p (variable &optional buffer)
  "Non-nil if VARIABLE has a local binding in buffer BUFFER."
  )

(defun byte-code-function-p (object)
  )

(defun defalias (symbol definition &optional docstring)
  "Set SYMBOL's function definition to DEFINITION, and return DEFINITION.
  Associates the function with the current load file, if any.
  The optional third argument DOCSTRING specifies the documentation string
  for SYMBOL; if it is omitted or nil, SYMBOL uses the documentation string"
  )

(defun setplist (symbol newplist)
  )

(defun set-default (symbol value)
  "Set SYMBOL's default value to VALUE.  SYMBOL and VALUE are evaluated.
  The default value is seen in buffers that do not have their own values"
  )

(defun symbol-function (symbol)
  )

(defun car (list)
  "Return the car of LIST.  If arg is nil, return nil.
  Error if arg is not nil and not a cons cell.  See also `car-safe'."
  )

(defun bool-vector-p (object)
  )

(defun subr-arity (subr)
  "Return minimum and maximum number of args allowed for SUBR.
  SUBR must be a built-in function.
  The returned value is a pair (MIN . MAX).  MIN is the minimum number
  of args.  MAX is the maximum number or the symbol `many', for a"
  )

(defun mod (x y)
  "Return X modulo Y.
  The result falls between zero (inclusive) and Y (exclusive)."
  )

(defun (core/symbol "1-") (number)
  "Return NUMBER minus one.  NUMBER may be a number or a marker."
  )

(defun atom (object)
  )

(defun null (object)
  "Return t if OBJECT is nil.if is a special form in `C source code'."
  )

(defun char-table-p (object)
  )

(defun number-to-string (number)
  "Return the decimal representation of NUMBER as a string.
  Uses a minus sign if negative."
  )

(defun integer-or-marker-p (object)
  )

(defun min (number-or-marker &rest numbers-or-markers)
  "Return smallest of all the arguments (which must be numbers or markers)."
  )

(defun string-to-number (string &optional base)
  "Parse STRING as a decimal number and return the number.
  This parses both integers and floating point numbers.
  It ignores leading spaces and tabs, and all trailing chars."
  )

(defun variable-binding-locus (variable)
  "Return a value indicating where VARIABLE's current binding comes from.
  If the current binding is buffer-local, the value is the current buffer.
  If the current binding is frame-local, the value is the selected frame."
  )

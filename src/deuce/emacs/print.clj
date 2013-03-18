(ns deuce.emacs.print
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c]
            [deuce.emacs.buffer :as buffer]
            [deuce.emacs.data :as data]
            [deuce.emacs.editfns :as editfns])
  (:refer-clojure :exclude [print]))

(defvar print-circle nil
  "*Non-nil means print recursive structures using #N= and #N# syntax.
  If nil, printing proceeds recursively and may lead to
  `max-lisp-eval-depth' being exceeded or an error may occur:
  \"Apparently circular structure being printed.\"  Also see
  `print-length' and `print-level'.
  If non-nil, shared substructures anywhere in the structure are printed
  with `#N=' before the first occurrence (in the order of the print
  representation) and `#N#' in place of each subsequent occurrence,
  where N is a positive decimal integer.")

(defvar print-gensym nil
  "Non-nil means print uninterned symbols so they will read as uninterned.
  I.e., the value of (make-symbol \"foobar\") prints as #:foobar.
  When the uninterned symbol appears within a recursive data structure,
  and the symbol appears more than once, in addition use the #N# and #N=
  constructs as needed, so that multiple references to the same symbol are
  shared once again when the text is read back.")

(defvar print-charset-text-property nil
  "A flag to control printing of `charset' text property on printing a string.
  The value must be nil, t, or `default'.

  If the value is nil, don't print the text property `charset'.

  If the value is t, always print the text property `charset'.

  If the value is `default', print the text property `charset' only when
  the value is different from what is guessed in the current charset
  priorities.")

(defvar print-escape-multibyte nil
  "Non-nil means print multibyte characters in strings as \\xXXXX.
  (XXXX is the hex representation of the character code.)
  This affects only `prin1'.")

(defvar standard-output nil
  "Output stream `print' uses by default for outputting a character.
  This may be any function of one argument.
  It may also be a buffer (output is inserted before point)
  or a marker (output is inserted and the marker is advanced)
  or the symbol t (output appears in the echo area).")

(defvar float-output-format nil
  "The format descriptor string used to print floats.
  This is a %-spec like those accepted by `printf' in C,
  but with some restrictions.  It must start with the two characters `%.'.
  After that comes an integer precision specification,
  and then a letter which controls the format.
  The letters allowed are `e', `f' and `g'.
  Use `e' for exponential notation \"DIG.DIGITSeEXPT\"
  Use `f' for decimal point notation \"DIGITS.DIGITS\".
  Use `g' to choose the shorter of those two formats for the number at hand.
  The precision in any of these cases is the number of digits following
  the decimal point.  With `f', a precision of 0 means to omit the
  decimal point.  0 is not allowed with `e' or `g'.

  A value of nil means to use the shortest notation
  that represents the number without losing information.")

(defvar print-escape-newlines nil
  "Non-nil means print newlines in strings as `\\n'.
  Also print formfeeds as `\\f'.")

(defvar print-quoted nil
  "Non-nil means print quoted forms with reader syntax.
  I.e., (quote foo) prints as 'foo, (function foo) as #'foo.")

(defvar print-continuous-numbering nil
  "*Non-nil means number continuously across print calls.
  This affects the numbers printed for #N= labels and #M# references.
  See also `print-circle', `print-gensym', and `print-number-table'.
  This variable should not be set with `setq'; bind it with a `let' instead.")

(defvar print-level nil
  "Maximum depth of list nesting to print before abbreviating.
  A value of nil means no limit.  See also `eval-expression-print-level'.")

(defvar print-number-table nil
  "A vector used internally to produce `#N=' labels and `#N#' references.
  The Lisp printer uses this vector to detect Lisp objects referenced more
  than once.

  When you bind `print-continuous-numbering' to t, you should probably
  also bind `print-number-table' to nil.  This ensures that the value of
  `print-number-table' can be garbage-collected once the printing is
  done.  If all elements of `print-number-table' are nil, it means that
  the printing done so far has not found any shared structure or objects
  that need to be recorded in the table.")

(defvar print-length nil
  "Maximum length of list to print before abbreviating.
  A value of nil means no limit.  See also `eval-expression-print-length'.")

(defvar print-escape-nonascii nil
  "Non-nil means print unibyte non-ASCII chars in strings as \\OOO.
  (OOO is the octal representation of the character code.)
  Only single-byte characters are affected, and only in `prin1'.
  When the output goes in a multibyte buffer, this feature is
  enabled regardless of the value of the variable.")

(defun error-message-string (obj)
  "Convert an error value (ERROR-SYMBOL . DATA) to an error message.
  See Info anchor `(elisp)Definition of signal' for some details on how this
  error message is constructed."
  )

(defun print (object &optional printcharfun)
  "Output the printed representation of OBJECT, with newlines around it.
  Quoting characters are printed when needed to make output that `read'
  can handle, whenever this is possible.  For complex objects, the behavior
  is controlled by `print-level' and `print-length', which see.

  OBJECT is any of the Lisp data types: a number, a string, a symbol,
  a list, a buffer, a window, a frame, etc.

  A printed representation of an object is text which describes that object.

  Optional argument PRINTCHARFUN is the output stream, which can be one
  of these:

     - a buffer, in which case output is inserted into that buffer at point;
     - a marker, in which case output is inserted at marker's position;
     - a function, in which case that function is called once for each
       character of OBJECT's printed representation;
     - a symbol, in which case that symbol's function definition is called; or
     - t, in which case the output is displayed in the echo area.

  If PRINTCHARFUN is omitted, the value of `standard-output' (which see)
  is used instead."
  (println object)
  object)

(defun redirect-debugging-output (file &optional append)
  "Redirect debugging output (stderr stream) to file FILE.
  If FILE is nil, reset target to the initial stderr stream.
  Optional arg APPEND non-nil (interactively, with prefix arg) means
  append to existing target file."
  )

(defun terpri (&optional printcharfun)
  "Output a newline to stream PRINTCHARFUN.
  If PRINTCHARFUN is omitted or nil, the value of `standard-output' is used."
  (println)
  true)

(defun prin1-to-string (object &optional noescape)
  "Return a string containing the printed representation of OBJECT.
  OBJECT can be any Lisp object.  This function outputs quoting characters
  when necessary to make output that `read' can handle, whenever possible,
  unless the optional second argument NOESCAPE is non-nil.  For complex objects,
  the behavior is controlled by `print-level' and `print-length', which see.

  OBJECT is any of the Lisp data types: a number, a string, a symbol,
  a list, a buffer, a window, a frame, etc.

  A pqrinted representation of an object is text which describes that object."
  (pr-str object))

(defun prin1 (object &optional printcharfun)
  "Output the printed representation of OBJECT, any Lisp object.
  Quoting characters are printed when needed to make output that `read'
  can handle, whenever this is possible.  For complex objects, the behavior
  is controlled by `print-level' and `print-length', which see.

  OBJECT is any of the Lisp data types: a number, a string, a symbol,
  a list, a buffer, a window, a frame, etc.

  A printed representation of an object is text which describes that object.

  Optional argument PRINTCHARFUN is the output stream, which can be one
  of these:

     - a buffer, in which case output is inserted into that buffer at point;
     - a marker, in which case output is inserted at marker's position;
     - a function, in which case that function is called once for each
       character of OBJECT's printed representation;
     - a symbol, in which case that symbol's function definition is called; or
     - t, in which case the output is displayed in the echo area.

  If PRINTCHARFUN is omitted, the value of `standard-output' (which see)
  is used instead."
  (let [printcharfun (or printcharfun (data/symbol-value 'standard-output))
        s (prin1-to-string object)]
    (condp some [printcharfun]
      #{nil true} (editfns/message s)
      data/bufferp (binding [buffer/*current-buffer* printcharfun]
                     (editfns/insert s)))))

(defun external-debugging-output (character)
  "Write CHARACTER to stderr.
  You can call print while debugging emacs, and pass it this function
  to make it write to the debugging output."
  )

(defun princ (object &optional printcharfun)
  "Output the printed representation of OBJECT, any Lisp object.
  No quoting characters are used; no delimiters are printed around
  the contents of strings.

  OBJECT is any of the Lisp data types: a number, a string, a symbol,
  a list, a buffer, a window, a frame, etc.

  A printed representation of an object is text which describes that object.

  Optional argument PRINTCHARFUN is the output stream, which can be one
  of these:

     - a buffer, in which case output is inserted into that buffer at point;
     - a marker, in which case output is inserted at marker's position;
     - a function, in which case that function is called once for each
       character of OBJECT's printed representation;
     - a symbol, in which case that symbol's function definition is called; or
     - t, in which case the output is displayed in the echo area.

  If PRINTCHARFUN is omitted, the value of `standard-output' (which see)
  is used instead."
  )

(defun write-char (character &optional printcharfun)
  "Output character CHARACTER to stream PRINTCHARFUN.
  PRINTCHARFUN defaults to the value of `standard-output' (which see)."
  )

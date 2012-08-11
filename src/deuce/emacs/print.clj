(ns
 deuce.emacs.print
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude [print]))

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
  )

(defun redirect-debugging-output (file &optional append)
  "Redirect debugging output (stderr stream) to file FILE.
  If FILE is nil, reset target to the initial stderr stream.
  Optional arg APPEND non-nil (interactively, with prefix arg) means
  append to existing target file."
  )

(defun terpri (&optional printcharfun)
  "Output a newline to stream PRINTCHARFUN.
  If PRINTCHARFUN is omitted or nil, the value of `standard-output' is used."
  )

(defun prin1-to-string (object &optional noescape)
  "Return a string containing the printed representation of OBJECT.
  OBJECT can be any Lisp object.  This function outputs quoting characters
  when necessary to make output that `read' can handle, whenever possible,
  unless the optional second argument NOESCAPE is non-nil.  For complex objects,
  the behavior is controlled by `print-level' and `print-length', which see.
  
  OBJECT is any of the Lisp data types: a number, a string, a symbol,
  a list, a buffer, a window, a frame, etc.
  
  A printed representation of an object is text which describes that object."
  )

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
  )

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

(ns emacs.print (use [deuce.core]) (:refer-clojure :only []))

(defun error-message-string (obj)
  "Convert an error value (ERROR-SYMBOL . DATA) to an error message.\nSee Info anchor `(elisp)Definition of signal' for some details on how this\n"
  )

(defun print (object &optional printcharfun)
  "Output the printed representation of OBJECT, with newlines around it.\nQuoting characters are printed when needed to make output that `read'\ncan handle, whenever this is possible.  For complex objects, the behavior\nis controlled by `print-level' and `print-length', which see."
  )

(defun terpri (&optional printcharfun)
  "Output a newline to stream PRINTCHARFUN.\n"
  )

(defun prin1-to-string (object &optional noescape)
  "Return a string containing the printed representation of OBJECT.\nOBJECT can be any Lisp object.  This function outputs quoting characters\nwhen necessary to make output that `read' can handle, whenever possible,\nunless the optional second argument NOESCAPE is non-nil.  For complex objects,\nthe behavior is controlled by `print-level' and `print-length', which see."
  )

(defun prin1 (object &optional printcharfun)
  "Output the printed representation of OBJECT, any Lisp object.\nQuoting characters are printed when needed to make output that `read'\ncan handle, whenever this is possible.  For complex objects, the behavior\nis controlled by `print-level' and `print-length', which see."
  )

(defun external-debugging-output (character)
  "Write CHARACTER to stderr.\nYou can call print while debugging emacs, and pass it this function\n"
  )

(defun princ (object &optional printcharfun)
  "Output the printed representation of OBJECT, any Lisp object.\nNo quoting characters are used; no delimiters are printed around\nthe contents of strings."
  )

(defun write-char (character &optional printcharfun)
  "Output character CHARACTER to stream PRINTCHARFUN.\n"
  )

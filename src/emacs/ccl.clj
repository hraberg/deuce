(ns emacs.ccl (use [deuce.core]) (:refer-clojure :only []))

(defun register-ccl-program (name ccl-prog)
  "Register CCL program CCL-PROG as NAME in `ccl-program-table'.\nCCL-PROG should be a compiled CCL program (vector), or nil.\nIf it is nil, just reserve NAME as a CCL program name.\n"
  )

(defun ccl-execute (ccl-prog reg)
  "Execute CCL-PROGRAM with registers initialized by REGISTERS."
  )

(defun ccl-program-p (object)
  "Return t if OBJECT is a CCL program name or a compiled CCL program code.\n"
  )

(defun ccl-execute-on-string (ccl-program status string &optional continue unibyte-p)
  "Execute CCL-PROGRAM with initial STATUS on STRING."
  )

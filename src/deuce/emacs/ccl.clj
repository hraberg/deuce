(ns
 deuce.emacs.ccl
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun register-ccl-program (name ccl-prog)
  "Register CCL program CCL-PROG as NAME in `ccl-program-table'.
  CCL-PROG should be a compiled CCL program (vector), or nil.
  If it is nil, just reserve NAME as a CCL program name.
  Return index number of the registered CCL program."
  )

(defun ccl-execute (ccl-prog reg)
  "Execute CCL-PROGRAM with registers initialized by REGISTERS."
  )

(defun ccl-program-p (object)
  "Return t if OBJECT is a CCL program name or a compiled CCL program code.
  See the documentation of `define-ccl-program' for the detail of CCL program."
  )

(defun register-code-conversion-map (symbol map)
  "Register SYMBOL as code conversion map MAP.
  Return index number of the registered map."
  )

(defun ccl-execute-on-string (ccl-program status string &optional continue unibyte-p)
  "Execute CCL-PROGRAM with initial STATUS on STRING."
  )

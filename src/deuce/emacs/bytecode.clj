(ns
 deuce.emacs.bytecode
 (:use [deuce.emacs-lisp :only (defun defvar)])
 (:refer-clojure :exclude []))

(defun byte-code (bytestr vector maxdepth)
  "Function used internally in byte-compiled code.
  The first argument, BYTESTR, is a string of byte code;
  the second, VECTOR, a vector of constants;
  the third, MAXDEPTH, the maximum stack depth used in this function.
  If the third argument is incorrect, Emacs may crash."
  )

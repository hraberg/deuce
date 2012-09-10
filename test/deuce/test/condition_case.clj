(ns deuce.test.condition-case
  (:use [deuce.test.common]))

;; "10.5.3.3 Writing Code to Handle Errors"[1]

;; Also forced out a version of / for Emacs, which rounds depending on the argument.
;; Hints that the scaffolded primitives should be Clojure defns, not Emacs Lisp defuns.
;; EmacsLispError is also a bit unclear in it's use.

;; [1] http://www.gnu.org/software/emacs/manual/html_node/elisp/Handling-Errors.html

(with-fresh-emacs)

(repl example-1
      (defun safe-divide (dividend divisor)
        (condition-case err
           (/ dividend divisor)
         (arith-error
           1000000)))           ⇒ 'safe-divide

     (safe-divide 5 0)          ⇒ 1000000

     (safe-divide nil 3)        ⇒ Exception)
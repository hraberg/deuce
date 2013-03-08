(ns deuce.test.condition-case
  (:use [deuce.test.common]))

;; "10.5.3.3 Writing Code to Handle Errors"[1]

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
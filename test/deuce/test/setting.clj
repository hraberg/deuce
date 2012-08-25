(ns deuce.test.setting
  (use [deuce.test.common]))

;; "11.8 Setting Variable Values"[1]

;; The fn set uses eval internally to expand the macro setq.
;; This heavy reliance on eval might mean that the model for Emacs Lisp is slightly off.

;; [1] http://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html

(with-fresh-emacs)

(repl example-1

      (setq x (+ 1 2))  ⇒ 3

      x                 ⇒ 3

      (let ((x 5))
        (setq x 6)
        x)              ⇒ 6

      x                 ⇒ 3

      (setq x 10
            y (+ 1 x))  ⇒ 11)


(repl example-2

      (set one 1)       ⇒ Exception

      (set 'one 1)      ⇒ 1

      (set 'two 'one)   ⇒ 'one

      (set two 2)       ⇒ 2

      one               ⇒ 2

      (let ((one 1))
         (set 'one 3)
         one)           ⇒ 3

      one               ⇒ 2

      (set '(x y) 'z)   ⇒ Exception)

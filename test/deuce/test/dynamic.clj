(ns deuce.test.dynamic
  (use [deuce.test.common]))

;; "11.9.1 Dynamic Binding"[1]

;; Uses binding + var-set/alter-var-root, but not sure about this approach - at least it works for this example.

;; [1] http://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Binding.html

(with-fresh-emacs)

(repl example-1

      (defvar x -99) ⇒ 'x

      (defun getx ()
        x)           ⇒ 'getx

      (let ((x 1))
        (getx))      ⇒ 1

      (getx)         ⇒ -99)


(repl example-2

      (defvar x -99)

      (defun addx ()
        (setq x (+ 1 x)))

      (let ((x 1))
        (addx)
        (addx))      ⇒ 3

      (addx)         ⇒ -98)
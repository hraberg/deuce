(ns deuce.test.locals
  (:use [deuce.test.common]))

;; "11.3 Local Variables"[1]

;; [1] http://www.gnu.org/software/emacs/manual/html_node/elisp/Local-Variables.html

(with-fresh-emacs)

(repl example-1

      (setq y 2)    ⇒ 2

      (let ((y 1)
            (z y))
        (list y z)) ⇒ [1 2])

(repl example-2

     (setq y 2)     ⇒ 2

     (let* ((y 1)
            (z y))
        (list y z)) ⇒ [1 1])
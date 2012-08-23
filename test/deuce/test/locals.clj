(ns deuce.test.dynamic
  (use [deuce.test.common]))

;; "11.3 Local Variables"[1]

;; In Emacs, let* allows you to refer back to bindings in the same let*, like in Clojure.
;; Normal let does not.

;; [1] http://www.gnu.org/software/emacs/manual/html_node/elisp/Local-Variables.html

(with-fresh-emacs)

(repl example-1

      (setq y 2)    ⇒ 2

      (let ((y 1)
            (z y))
        (list y z)) ⇒ '(1 2))

(repl example-2

     (setq y 2)     ⇒ 2

     (let* ((y 1)
            (z y))
        (list y z)) ⇒ '(1 1))
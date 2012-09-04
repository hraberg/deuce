(ns deuce.test.defining-macros
  (use [deuce.test.common]))

;; "13.4 Defining Macros"[1]

;; Also tests that the read/eval combo handles Emacs Lisp quoting with ','.

;; [1] http://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Macros.html

(with-fresh-emacs)

(repl example-1

      (eval (read "(defmacro t-becomes-nil (variable)
              `(if (eq ,variable t)
                   (setq ,variable nil)))")) ⇒ 't-becomes-nil

      (t-becomes-nil a)                      ⇒ Exception

      (setq a 2)
      (t-becomes-nil a)
      a                                      ⇒ 2

      (setq a t)                             ⇒ true
      (t-becomes-nil a)

      a                                      ⇒ nil)

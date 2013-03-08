(ns deuce.test.catch
  (:use [deuce.test.common]))

;; "5 Lists"[1]

;; [1] http://www.gnu.org/software/emacs/manual/html_node/elisp/Lists.html

(with-fresh-emacs)

(repl predicates-on-lists

      (listp '(1))   ⇒ true

      (listp ())     ⇒ true

      (null '(1))    ⇒ nil

      (null ()       ⇒ true))
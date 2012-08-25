(ns deuce.test.catch
  (use [deuce.test.common]))

;; "10.5.2 Examples of catch and throw"[1]

;; Uses an exception defined in Java, deuce.EmacsLispError.
;; The way the tag gets evaluated internally is likely wrong.

;; [1] http://www.gnu.org/software/emacs/manual/html_node/elisp/Examples-of-Catch.html

(with-fresh-emacs)

(repl example-2

      (defun catch2 (tag)
        (catch tag
          (throw 'hack 'yes)))  ⇒ 'catch2

      (catch 'hack
        (catch2 'hack)
        'no)                    ⇒ 'no


      (catch 'hack
        (catch2 'quux)
        'no)                    ⇒ 'yes)
(ns deuce.test.dynamic
  (require [deuce.emacs-lisp :as el])
  (use [clojure.test]))

;; This is a way to represent the examples from "11.9.1 Dynamic Binding"[1] in Clojure.

;; Uses binding + var-set/alter-var-root, but not sure about this approach - at least it works.
;; deuce.emacs-lisp attempts to compile setq to intern, but that's not enough, as Emacs Lisp variables are both mutable and rebindable.

;; [1] http://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Binding.html

(defmacro emacs [& body]
  `(last (map el/eval '~body)))

(deftest example-1
  (are [lisp result] (= result lisp)

       (emacs
        (defvar x -99)  ; x receives an initial value of -99.

        (defun getx ()
          x)            ; x is used ``free'' in this function.

        (let ((x 1))    ; x is dynamically bound.
          (getx)))
       1

       ;; After the let form finishes, x reverts to its
       ;; previous value, which is -99.
       (emacs (getx))
       -99))

(deftest example-2
  (are [lisp result] (= result lisp)

       (emacs
        (defvar x -99) ; x receives an initial value of -99.

        (defun addx ()
          (setq x (+ 1 x)))

        (let ((x 1))
          (addx)
          (addx)))
       3               ; The two addx calls add to x twice.

       ;; After the let form finishes, x reverts to its
       ;; previous value, which is -99.
       (emacs (addx))
       -98))
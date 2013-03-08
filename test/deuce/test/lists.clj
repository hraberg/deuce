(ns deuce.test.lists
  (:use [deuce.test.common]))

;; "5 Lists"[1]

;; [1] http://www.gnu.org/software/emacs/manual/html_node/elisp/Lists.html

(with-fresh-emacs)

(repl predicates-on-lists

      (listp '(1))   ⇒ true

      (listp '())    ⇒ true

      (null '(1))    ⇒ nil

      (null '()      ⇒ true))

(repl accessing-elements-on-lists

      (car '(a b c))         ⇒ 'a

      (cdr '())              ⇒ nil

      (nth 2  '(1 2 3 4))    ⇒ 3

      (nth 10 '(1 2 3 4))    ⇒ nil

      (nth -3 '(1 2 3 4))    ⇒ 1

      (nthcdr 1 '(1 2 3 4))  ⇒ '(2 3 4)

      (nthcdr 10 '(1 2 3 4)) ⇒ nil

      (nthcdr -3 '(1 2 3 4)) ⇒ '(1 2 3 4))

(repl building-cons-cells-and-lists
      (cons 1 '(2))                   ⇒ '(1 2)

      (cons 1 '())                    ⇒ '(1)

      (cons 1 2)                      ⇒ '(1 . 2)

      (list 1 2 3 4 5)                ⇒ '(1 2 3 4 5)

      (list 1 2 '(3 4 5) 'foo)        ⇒ '(1 2 (3 4 5) foo)

      (list)                          ⇒ nil

      (make-list 3 'pigs)             ⇒ '(pigs pigs pigs)

      (make-list 0 'pigs)             ⇒ nil

      (setq l (make-list 3 '(a b)))   ⇒ '((a b) (a b) (a b))

      (defun cadr (x) (car (cdr x)))

      (eq (car l) (cadr l))           ⇒ true

      (setq trees '(pine oak))        ⇒ '(pine oak)

      (setq more-trees (append '(maple birch) trees))
                                      ⇒ '(maple birch pine oak)

      trees                           ⇒ '(pine oak)

      more-trees                      ⇒ '(maple birch pine oak)

      (eq trees (cdr (cdr more-trees)))
                                      ⇒ true

      trees                           ⇒ '(pine oak)

      (setq wood (append trees nil))  ⇒ '(pine oak)

      wood                            ⇒ '(pine oak)

      (eq wood trees)                 ⇒ true

      (append #deuce/vector [a b] "cd" nil)
                                      ⇒ '(a b 99 100)

      (apply 'append '((a b c) nil (x y z) nil))
                                      ⇒ '(a b c x y z)

      (append)                        ⇒ nil

      (append '(x y) 'z)              ⇒ '(x y . z)

      (append '(x y) #deuce/vector [z])
                                     ⇒ '(x y . #deuce/vector [z])

      (setq x '(1 2 3 4))             ⇒ '(1 2 3 4)

      (reverse x)                     ⇒ '(4 3 2 1)

      x                               ⇒ '(1 2 3 4))
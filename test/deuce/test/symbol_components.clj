(ns deuce.test.symbol-components
  (:use [deuce.test.common]))

;; "8.1 Symbol Components"[1]

;; As Emacs Lisp is a Lisp-2, a symbol can be a function and a var at the same time.
;; Deuce currently cheats a bit here, and assumes all symbols resolve into deuce.emacs-lisp.globals
;; Fns have to be resolvable from the deuce.emacs ns, and this is corrected for cars after macroexpand-all.
;; This isn't the best way, but wanted to try something simpler than how shen.clj (also a Lisp-2) does it.

;; [1] http://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Components.html

(with-fresh-emacs)

(repl example-1

      (setq buffer-file-name "/gnu/elisp/symbols.texi")

      buffer-file-name                     ⇒ "/gnu/elisp/symbols.texi"

      (symbol-function 'buffer-file-name)  ⇒ fn?

      (buffer-file-name)                   ⇒ "/gnu/elisp/symbols.texi"

      (symbol-value 'buffer-file-name)     ⇒ "/gnu/elisp/symbols.texi"

      'buffer-file-name                    ⇒ 'buffer-file-name)

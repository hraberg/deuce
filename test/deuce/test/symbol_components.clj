(ns deuce.test.symbol-components
  (:use [deuce.test.common]))

;; "8.1 Symbol Components"[1]

;; [1] http://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Components.html

(with-fresh-emacs)

(repl example-1

      (setq buffer-file-name "/gnu/elisp/symbols.texi")

      buffer-file-name                     ⇒ "/gnu/elisp/symbols.texi"

      (symbol-function 'buffer-file-name)  ⇒ fn?

      (buffer-file-name)                   ⇒ "/gnu/elisp/symbols.texi"

      (symbol-value 'buffer-file-name)     ⇒ "/gnu/elisp/symbols.texi"

      'buffer-file-name                    ⇒ 'buffer-file-name)

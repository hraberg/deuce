(ns leiningen.marg-el
  (require [marginalia.main]
           [marginalia.parser]))

(defn marg-el
  "Run Marginalia with Deuce Emacs Lisp macro support."
  [project & args]
  (defn emacs-lisp-doc [form raw nspace-sym]
    (let [doc (nth form 3 nil)]
      [doc (marginalia.parser/strip-docstring doc raw) nspace-sym]))

  (defmethod marginalia.parser/dispatch-form 'defun
    [form raw nspace-sym]
    (emacs-lisp-doc form raw nspace-sym))

  (defmethod marginalia.parser/dispatch-form 'defvar
    [form raw nspace-sym]
    (emacs-lisp-doc form raw nspace-sym))

  ;; Duplicated from deuce.emacs-lisp
  (defn symbol-reader [s]
    (symbol nil s))

  (binding [*data-readers* {'deuce/symbol #'symbol-reader}]
    (apply marginalia.main/-main args)))

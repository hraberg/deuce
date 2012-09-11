(ns leiningen.marg-el
  (require [marginalia.main]
           [marginalia.parser]))

(defn marg-el
  "Run Marginalia with Deuce Emacs Lisp macro support"
  [project & args]
  (defn emacs-lisp-doc [form raw nspace-sym]
    (let [doc (nth form 3)]
      [doc (marginalia.parser/strip-docstring doc raw) nspace-sym]))

  (defmethod marginalia.parser/dispatch-form 'defun
    [form raw nspace-sym]
    (emacs-lisp-doc form raw nspace-sym))

  (defmethod marginalia.parser/dispatch-form 'defvar
    [form raw nspace-sym]
    (emacs-lisp-doc form raw nspace-sym))

  (apply marginalia.main/-main args))

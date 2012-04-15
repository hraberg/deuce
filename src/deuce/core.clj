(ns deuce.core
  (require [clojure.walk :as walk]))

(defmacro defun [name args & body]
  (let [[doc body] (split-with string? body)
        name (if (seq? name) (eval name) name)]
    `(do (defn ~name ~(vec args) ~@body)
         (alter-meta! (var ~name) merge {:doc ~(apply str doc)})
         ~name)))

(defmacro eval-in-emacs [& emacs-lisp]
  `(let [env# (zipmap '~(keys &env) ~(vec (keys &env)))
         code# (str "(progn " (apply str (walk/postwalk-replace env# '~emacs-lisp)) ")")]
     (:out (sh/sh "emacs/src/emacs" "-Q" "-batch" "--eval" code#))))

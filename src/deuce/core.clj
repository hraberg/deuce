(ns deuce.core
  (require [clojure.walk :as walk])
  (:gen-class))

(def t true)

(defmacro defun [name args & body]
  (let [[doc body] (split-with string? body)
        name (if (seq? name) (eval name) name)
        args (replace '{&rest &} args)
        [args &optional optional-args] (partition-by '#{&optional} args)
        args (concat args (when &optional ['& (vec optional-args)]))]
    `(do (defn ~name ~(vec args) ~@body)
         (alter-meta! (var ~name) merge {:doc ~(apply str doc)})
         ~name)))

(defmacro eval-in-emacs [& emacs-lisp]
  `(let [env# (zipmap '~(keys &env) ~(vec (keys &env)))
         code# (str "(progn " (apply str (walk/postwalk-replace env# '~emacs-lisp)) ")")]
     (:out (sh/sh "emacs/src/emacs" "-Q" "-batch" "--eval" code#))))

(defn -main [& args])
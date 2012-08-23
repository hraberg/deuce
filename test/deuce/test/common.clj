(ns deuce.test.common
  (require [deuce.emacs-lisp :as el])
  (use [clojure.test]))

(defmacro emacs [& body]
  `(last (map #(el/eval %) '~body)))

(defn remove-vars [ns vars]
  (dorun (map #(ns-unmap ns %) vars)))

(defn clear-globals []
  (remove-vars 'deuce.emacs-lisp.globals (keys (ns-publics 'deuce.emacs-lisp.globals))))

(defn with-fresh-emacs []
  (use-fixtures :each (fn [t]
                        (let [fns (set (keys (ns-publics 'deuce.emacs-lisp)))]
                          (t)
                          (remove-vars 'deuce.emacs-lisp (remove fns (keys (ns-publics 'deuce.emacs-lisp.globals))))
                          (clear-globals)))))

(defmacro repl [name & body]
  (let [parts (remove '#{[⇒]} (partition-by '#{⇒} body))
        expected (map first (next parts))
        parts (cons (first parts) (map rest (next parts)))]
    (concat `(deftest ~name)
            (for [[a e] (partition 2 (interleave parts expected))]
              (if (and (symbol? e) (instance? Throwable (resolve e)))
                `(is (~'thrown? ~e (emacs ~@a)))
                `(is ((if (fn? ~e) ~e #{~e}) (emacs ~@a))))))))

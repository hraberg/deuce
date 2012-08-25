(ns deuce.test.common
  (require [deuce.emacs-lisp :as el]
           [deuce.emacs])
  (use [clojure.test]))

(defmacro emacs [& body]
  `(last (map #(el/eval %) '~body)))

(defn remove-vars [ns vars]
  (dorun (map #(ns-unmap ns %) vars)))

(defn clear-publics [ns]
  (remove-vars ns (keys (ns-publics ns))))

(defn with-fresh-emacs []
  (use-fixtures :each (fn [t]
                        (let [clear #(dorun (map clear-publics '[deuce.emacs deuce.emacs-lisp.globals]))]
                          (clear)
                          (t)
                          (clear)))))

(defmacro repl [name & body]
  (let [parts (remove '#{[⇒]} (partition-by '#{⇒} body))
        expected (map first (next parts))
        parts (cons (first parts) (map rest (next parts)))]
    (concat `(deftest ~name)
            (for [[a e] (partition 2 (interleave parts expected))]
              (if (and (symbol? e) (isa? (resolve e) Throwable))
                `(is (~'thrown? ~e (emacs ~@a)))
                (if (and (symbol? e) (resolve e))
                  `(is (~(resolve e) (emacs ~@a)))
                  `(is (= ~e (emacs ~@a)))))))))

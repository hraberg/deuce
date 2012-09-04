(ns deuce.test.common
  (require [deuce.emacs-lisp :as el]
           [deuce.emacs])
  (use [clojure.test]))

(defmacro emacs [& body]
  `(last (map #(el/eval %) '~body)))

(defn remove-vars [ns vars]
  (dorun (map #(ns-unmap ns %) vars)))

(defn clear-publics [ns keep]
  (remove-vars ns (remove keep (keys (ns-publics ns)))))

(defn with-fresh-emacs []
  (use-fixtures :each (fn [t]
                        (let [[fns vars] (map ns-map '[deuce.emacs deuce.emacs-lisp.globals])
                              previous-vars (->> (ns-map 'deuce.emacs-lisp.globals)
                                                 (filter (comp var? val))
                                                 (map (fn [[n v]] [n (deref v)]))
                                                 (into {}))]

                          (t)

                          (clear-publics 'deuce.emacs fns)
                          (clear-publics 'deuce.emacs-lisp.globals vars)
                          (->> previous-vars
                               (map (fn [[n v]] (alter-var-root (ns-resolve 'deuce.emacs-lisp.globals n)
                                                                (constantly v))))
                               doall)))))

(defmacro repl [name & body]
  (let [parts (partition-by '#{⇒ -|} body)
        ops (filter '#{⇒ -|} body)
        parts (remove '#{[⇒] [-|]} parts)
        expected (map first (next parts))
        parts (cons (first parts) (map rest (next parts)))]
    (concat `(deftest ~name)
            (for [[a op e] (partition 3 (interleave parts ops expected))]
              (if (and (symbol? e) (isa? (resolve e) Throwable))
                `(is (~'thrown? ~e (emacs ~@a)))
                (if (= '-| op)
                  `(is (re-find (re-pattern ~e) (with-out-str (emacs ~@a))))
                  (cond
                   (and (symbol? e) (resolve e)) `(is (~(resolve e) (emacs ~@a)))
                   (instance? java.util.regex.Pattern e) `(is (re-find (re-pattern ~e) (emacs ~@a)))
                   :else `(is (= ~e (emacs ~@a))))))))))

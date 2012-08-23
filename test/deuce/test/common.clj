(ns deuce.test.common
  (require [deuce.emacs-lisp :as el])
  (use [clojure.test]))

(defmacro emacs [& body]
  `(last (map el/eval '~body)))

(defmacro repl [name & body]
  (let [parts (remove '#{[⇒]} (partition-by '#{⇒} body))
        expected (map first (next parts))
        parts (cons (first parts) (map rest (next parts)))]
    (concat `(deftest ~name)
            (for [[a e] (partition 2 (interleave parts expected))]
              `(is (= (emacs ~@a) ~e))))))

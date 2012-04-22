(ns deuce.test.core
  (:use [deuce.core])
  (:use [clojure.test])
  (import [java.util Scanner]
          [java.io StringReader]))

(deftest replace-me ;; FIXME: write
  (is false "No tests have been written."))

(defn tokenize [sc]
  (let [look #(.findWithinHorizon sc (pr-str %) 1)
        tokenize-all #(take-while identity (repeatedly (partial tokenize sc)))
        unquote #(if (look \@) 'unquote-splicing 'unquote)
        re-sym #"[\w\d-+/*<>=.]+"]
    (cond
     (look \s) (recur sc)
     (look \() (apply list (tokenize-all))
     (look \[) (vec (tokenize-all))
     (look \,) (list (unquote) (tokenize sc))
     (.hasNextBigDecimal sc) (.nextBigDecimal sc)
     (.hasNext sc re-sym) (symbol (.next sc re-sym))
     (.hasNext sc) (not-empty (.next sc)))))

(defn parse [r]
  (tokenize (doto (Scanner. (if (string? r) (StringReader. r) r))
              (.useDelimiter #"(\ +|\]|\))"))))

(ns deuce.test.core
  (:use [deuce.core])
  (:use [clojure.test])
  (:require [clojure.string :as string])
  (import [java.util Scanner]
          [java.io StringReader]))

(deftest replace-me ;; FIXME: write
  (is false "No tests have been written."))

(defn tokenize [sc]
  (let [find #(.findWithinHorizon sc %1 %2)
        look #(find (pr-str %) 1)
        tokenize-all #(take-while identity (repeatedly (partial tokenize sc)))
        unquote #(if (look \@) 'unquote-splicing 'unquote)
        re-sym #"[\w\d-+/*<>=.]+"
        re-str #"(?:[^\"\\]|\\.)*\""]
    (cond
     (look \s) (recur sc)
     (look \() (apply list (tokenize-all))
     (look \[) (vec (tokenize-all))
     (look \,) (list (unquote) (tokenize sc))
     (look \") (string/replace (->> (find re-str 0) drop-last (apply str))
                               #"\\(.)" "$1")
     (.hasNextBigDecimal sc) (.nextBigDecimal sc)
     (.hasNext sc re-sym) (symbol (.next sc re-sym))
     (.hasNext sc) (not-empty (.next sc)))))

(defn parse [r]
  (tokenize (doto (Scanner. (if (string? r) (StringReader. r) r))
              (.useDelimiter #"( +|\]|\))"))))

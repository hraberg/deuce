(ns deuce.test.core
  (:use [deuce.core])
  (:use [clojure.test])
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (import [java.util Scanner]
          [java.io StringReader]))

(deftest replace-me ;; FIXME: write
  (is false "No tests have been written."))

(declare tokenize)

(defn tokenize-all [sc]
  (take-while identity (repeatedly (partial tokenize sc))))

(defn tokenize [sc]
  (let [find #(.findWithinHorizon sc %1 %2)
        look #(find (pr-str %) 1)
        unquote #(if (look \@) 'unquote-splicing 'unquote)
        re-sym #"[\S&&[^~`#\'\"]]+"
        re-str #"(?:[^\"\\]|\\.)*\""
        re-chr #"[\S&&[^\]\)\s]]*"]
    (cond
     (look \s) (recur sc)
     (look \() (apply list (tokenize-all sc))
     (look \[) (vec (tokenize-all sc))
     (or (look \)) (look \])) nil
     (look \,) (list (unquote) (tokenize sc))
     (look \') (list 'quote (tokenize sc))
     (look \`) (list 'syntax-quote (tokenize sc))
     (look \:) (keyword (tokenize sc))
     (look \?) (symbol (str \? (find re-chr 0)))
     (look \") (string/replace (->> (find re-str 0) drop-last (apply str))
                               #"\\(.)" "$1")
     (look \;) (list 'comment (.nextLine sc))
     (and (look \#) (look \')) (list 'var (tokenize sc))
     (.hasNextLong sc) (.nextLong sc)
     (.hasNextDouble sc) (.nextDouble sc)
     (.hasNext sc re-sym) (symbol (.next sc re-sym))
     (.hasNext sc) (when-let [x (not-empty (tokenize sc))]
                     (assert false (str "unexpected: " (apply str x)))))))

(defn parse [r]
  (tokenize-all (doto (Scanner. (if (string? r) (StringReader. r) r))
                  (.useDelimiter #"(\s+|\]|\))"))))

(defn smoke []
  (doseq [el (filter #(re-find #".el$" (str %)) (file-seq (io/file "emacs/lisp")))]
    (with-open [r (io/reader el)]
      (println el (try (count (parse r)) (catch Throwable e e))))))

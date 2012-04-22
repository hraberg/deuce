(ns deuce.test.core
  (:use [deuce.core])
  (:use [clojure.test])
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (import [java.util Scanner]
          [java.io StringReader]
          [java.util.regex Pattern]))

(declare tokenize)

(def ^Pattern re-sym #"[\S&&[^~`#\'\"]]+")
(def ^Pattern re-str #"(?:[^\"\\]|\\.)*\"")
(def ^Pattern re-chr #"[\S&&[^\]\)\s]]*")

(defn tokenize-all [^Scanner sc]
  (take-while identity (repeatedly (partial tokenize sc))))

(defn tokenize [^Scanner sc]
  (let [find (fn [^Pattern re h] (.findWithinHorizon sc re (int h)))
        look #(find (re-pattern (pr-str %)) 1)
        unquote #(if (look \@) 'unquote-splicing 'unquote)]
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
     (look \#) (if (look \')
                 (list 'var (tokenize sc))
                 (let [[object start end properties] (tokenize sc)]
                   (list 'set-text-properties start end properties object)))
     (.hasNextLong sc) (.nextLong sc)
     (.hasNextDouble sc) (.nextDouble sc)
     (.hasNext sc re-sym) (symbol (.next sc re-sym))
     (.hasNext sc) (when-let [x (not-empty (tokenize sc))]
                     (assert false (str "unexpected: " (apply str x)))))))

(defn parse [r]
  (tokenize-all (doto (Scanner. r)
                  (.useDelimiter #"(\s+|\]|\))"))))

(defn smoke []
  (doseq [el (filter #(re-find #".el$" (str %)) (file-seq (io/file "emacs/lisp")))]
    (println el (try (count (parse el)) (catch Throwable e e)))))

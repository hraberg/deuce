(ns deuce.test.core
  (:use [deuce.core])
  (:use [clojure.test])
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (import [java.util Scanner]
          [java.io StringReader StreamTokenizer]
          [java.util.regex Pattern]))

(declare tokenize)

(def ^Pattern re-sym #"[\S&&[^#\"]]+")
(def ^Pattern re-str #"(?s)([^\"\\]*(?:\\.[^\"\\]*)*)\"")
(def ^Pattern re-chr #"[\S&&[^\]\)\s]]*")

(defn tokenize-all [^Scanner sc]
  (take-while identity (repeatedly (partial tokenize sc))))

(defn tokenize [^Scanner sc]
  (let [find (fn [^Pattern re h] (.findWithinHorizon sc re (int h)))]
    (condp find 1
      #"\s" (recur sc)
      #"[)\]]" nil
      #"\(" (apply list (tokenize-all sc))
      #"\[" (vec (tokenize-all sc))
      #"," (list (if (find #"@" 1) 'unquote-splicing 'unquote) (tokenize sc))
      #"'" (list 'quote (tokenize sc))
      #"`" (list 'syntax-quote (tokenize sc))
      #":" (keyword (tokenize sc))
      #"\?" (symbol (str \? (find re-chr 0)))
      #"\"" (.sval (doto (StreamTokenizer. (StringReader. (str \" (find re-str 0))))
                     (.nextToken)))
      #";" (list 'comment (.nextLine sc))
      #"#" (condp find 1
             #"'" (list 'var (tokenize sc))
             #"x" (.nextInt sc (int 16))
             #"o" (.nextInt sc (int 8))
             #"b" (.nextInt sc (int 2))
             #"\(" (let [[object start end properties] (tokenize-all sc)]
                     (list 'set-text-properties start end properties object))
             (when (.hasNext sc #"\d+r\S+")
               (let [radix (find #"\d+" 0)]
                 (find #"r" 1)
                 (.nextInt sc (Integer/parseInt radix)))))
      (cond
       (.hasNextLong sc) (.nextLong sc)
       (.hasNextDouble sc) (.nextDouble sc)
       (.hasNext sc re-sym) (symbol (.next sc re-sym))
       (.hasNext sc) (assert false (str "unexpected: " (.next sc)))))))

(defn parse [r]
  (tokenize-all (doto (if (string? r) (Scanner. r) (Scanner. r "UTF-8"))
                  (.useDelimiter #"(\s+|\]|\)|\"|;)"))))

(defn smoke []
  (doseq [el (filter #(re-find #".el$" (str %)) (file-seq (io/file "emacs/lisp")))]
    (try (count (parse el)) (catch Throwable e (println el e)))))

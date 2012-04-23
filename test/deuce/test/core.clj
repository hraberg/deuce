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
  (let [find (fn [^Pattern re h] (.findWithinHorizon sc re (int h)))
        look #(find % 1)
        unquote #(if (look #"@") 'unquote-splicing 'unquote)]
    (cond
     (look #"\s") (recur sc)
     (look #"\(") (apply list (tokenize-all sc))
     (look #"\[") (vec (tokenize-all sc))
     (look #"[)\]]") nil
     (look #",") (list (unquote) (tokenize sc))
     (look #"'") (list 'quote (tokenize sc))
     (look #"`") (list 'syntax-quote (tokenize sc))
     (look #":") (keyword (tokenize sc))
     (look #"\?") (symbol (str \? (find re-chr 0)))
     (look #"\"") (.sval (doto (StreamTokenizer. (StringReader. (str \" (find re-str 0))))
                           (.nextToken)))
     (look #";") (list 'comment (.nextLine sc))
     (look #"#") (cond
                  (look #"'") (list 'var (tokenize sc))
                  (look #"x") (.nextInt sc (int 16))
                  (look #"o") (.nextInt sc (int 8))
                  (look #"b") (.nextInt sc (int 2))
                  (.hasNext sc #"\d+r\S+") (let [radix (find #"\d+" 0)]
                                             (look #"r")
                                             (.nextInt sc (Integer/parseInt radix)))
                (look #"\(") (let [[object start end properties] (tokenize-all sc)]
                            (list 'set-text-properties start end properties object)))
     (.hasNextLong sc) (.nextLong sc)
     (.hasNextDouble sc) (.nextDouble sc)
     (.hasNext sc re-sym) (symbol (.next sc re-sym))
     (.hasNext sc) (when-let [x (not-empty (tokenize sc))]
                     (assert false (str "unexpected: " (apply str x)))))))

(defn parse [r]
  (tokenize-all (doto (if (string? r) (Scanner. r) (Scanner. r "UTF-8"))
                  (.useDelimiter #"(\s+|\]|\)|\"|;)"))))

(defn smoke []
  (doseq [el (filter #(re-find #".el$" (str %)) (file-seq (io/file "emacs/lisp")))]
    (try (count (parse el)) (catch Throwable e (println el e)))))

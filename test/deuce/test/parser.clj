(ns deuce.test.parser
  (use [clojure.test])
  (require [clojure.string :as string]
           [clojure.java.io :as io])
  (import [java.util Scanner]
          [java.io StringReader StreamTokenizer]
          [java.util.regex Pattern]))

;; This is an Emacs Lisp parser spike.

;;   lein run -m deuce.test.parser

;; It can read all the .el files under emacs/lisp, but the actual representation as Clojure forms will probably change.

(declare tokenize)

(def ^Pattern re-str #"(?s)([^\"\\]*(?:\\.[^\"\\]*)*)\"")

(defn tokenize-all [^Scanner sc]
  (take-while identity (repeatedly (partial tokenize sc))))

(defn tokenize [^Scanner sc]
  (let [find (fn [^Pattern re h] (.findWithinHorizon sc re (int h)))]
    (condp find 1
      #"\s" (recur sc)
      #"[)\]]" nil
      #"\(" (apply list (tokenize-all sc))
      #"\[" (vec (tokenize-all sc))
      #"," (list (if (find #"@" 1) (symbol "\\,@") (symbol "\\,")) (tokenize sc))
      #"'" (list 'quote (tokenize sc))
      #"`" (list '\` (tokenize sc))
      #":" (keyword (.next sc))
      #"\?" (symbol (str \? (.next sc)))
      #"\"" (.sval (doto (StreamTokenizer. (StringReader. (str \" (find re-str 0))))
                     (.nextToken)))
      #";" (list 'comment (.nextLine sc))
      #"#" (condp find 1
             #"'" (list 'var (tokenize sc))
             #"\(" (let [[object start end properties] (tokenize-all sc)]
                     (list 'set-text-properties start end properties object))
             #"x" (.nextInt sc (int 16))
             #"o" (.nextInt sc (int 8))
             #"b" (.nextInt sc (int 2))
             (when (.hasNext sc #"\d+r\S+")
               (let [radix (find #"\d+" 0)]
                 (find #"r" 1)
                 (.nextInt sc (Integer/parseInt radix)))))
      (cond
       (.hasNextLong sc) (.nextLong sc)
       (.hasNextDouble sc) (.nextDouble sc)
       (.hasNext sc) (symbol (.next sc))))))

(defn parse [r]
  (tokenize-all (doto (if (string? r) (Scanner. r) (Scanner. r "UTF-8"))
                  (.useDelimiter #"(\s+|\]|\)|\"|;)"))))

(defn smoke []
  (doseq [el (filter #(re-find #".el$" (str %)) (file-seq (io/file "emacs/lisp")))]
    (try (println (str el) (count (parse el))) (catch Exception e (println e)))))

(defn -main []
  (println "Deuce Emacs Lisp Parser Smoke Test")
  (if (.exists (io/file "emacs/lisp"))
    (do
      (println "The numbers are number of top level Emacs Lisp forms parsed")
      (println "Takes about a minute...")
      (time (smoke)))
    (println "Cannot find Emacs, please run ./configure-emacs")))

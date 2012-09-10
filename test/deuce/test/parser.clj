(ns deuce.test.parser
  (:use [clojure.test]
        [deuce.emacs-lisp.parser])
  (:require [clojure.java.io :as io]))

;; This is an Emacs Lisp parser spike.

;;   lein run -m deuce.test.parser

;; It can read all the .el files under emacs/lisp, but the actual representation as Clojure forms will probably change.

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

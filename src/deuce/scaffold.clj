(ns deuce.scaffold
  (use [deuce.core :exclude (-main)])
  (require [clojure.java.shell :as sh]
           [clojure.string :as string]
           [clojure.walk :as walk]
           [clojure.java.io :as io])
  (:gen-class))

(defn read-subrs []
  (eval-in-emacs
   (require 'cl)

   (do-symbols (sym)
               (when
                   (and (fboundp sym) (subrp (symbol-function sym))
                        (princ (describe-function sym)))))))

(defn subrs []
  (->> (string/split (read-subrs) #".*is a built-in function in `C source code'.\n\n")
       (remove empty?)
       (map #(let [[decl doc] (->> (string/split % #"\n\n")
                                   (drop-while (complement (partial re-find #"\(.*"))))
                   [name & args] (string/split (subs decl 1 (dec (count decl))) #"\s+")]
               [(symbol name) {:args (map symbol args) :doc doc}]))
       (into {})))

(defn find-files-for-tags [tags]
  (sh/sh "./collect-tags")
  (let [tags (vec tags)]
    (->> (eval-in-emacs
          (setq tags-file-name "emacs/src/TAGS-TEMACS")
          (princ (mapcar (lambda (x) (buffer-name (find-tag-noselect x))) tags)))
         read-string
         (map #(string/replace % #"\..+$" "")))))

(defn generate-fn-stubs []
  (let [subrs (subrs)]
    (->> (map #(vector (key %) (assoc (val %) :namespace %2))
              subrs
              (find-files-for-tags (map name (keys subrs))))
         (into {})
         (group-by (comp :namespace val)))))

(def illegal-symbols {(symbol "1+") (symbol "1+")
                      (symbol "1-") (symbol "1-")
                      (symbol "/=") 'slash-equals
                      '... 'dot-dot-dot
                      'ARGS... 'args-dot-dot-dot})

(defn write-fn-stubs []
  (.mkdir (io/file "src/emacs"))
  (doseq [[original fns] (generate-fn-stubs)
          :let [namespace (symbol (str "emacs." original))]]
    (println namespace (str "(" (count fns) " subrs)"))
    (with-open [w (io/writer (io/file "src/emacs" (str original ".clj")))]
      (binding [*out* w]
        (println (list 'ns namespace
                       (list 'use ['deuce.core])
                       (list 'require ['clojure.core :as 'core])
                       (list :refer-clojure :only [])))
        (doseq [[f {:keys [args doc]}] fns]
          (println)
          (println (str "(defun " (if (illegal-symbols f) (str "(core/symbol \"" (str (illegal-symbols f)) "\")") f)
                        " " (pr-str (map (comp symbol string/lower-case)
                                         (replace illegal-symbols args)))))
          (when doc
            (print  "  \"")
            (print (-> doc
                       string/trimr
                       (string/replace #"\n" "\n  ")
                       (string/escape {\" "\\\"" \\ "\\\\"})))
            (println "\""))
          (println "  )"))))
    (require namespace)))

(defn -main []
  (write-fn-stubs)
  (System/exit 0))
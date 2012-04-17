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

(defn special-forms []
  (->> (string/split (read-subrs) #"\.|\)" )
       (map #(re-find #"(\S+) is a special form in `C source code'" %))
       (remove nil?)
       (map (comp symbol second))
       set))

(defn subrs []
  (->> (string/split (read-subrs) #"[^.)]+ is (an interactive built-in function|a built-in function|a special form)\s+in\s+(`C\ssource\scode'.\n\n|`subr.el'.)")
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

(def subr-aliases '#{search-forward-regexp search-backward-regexp})

(defn generate-fn-stubs []
  (let [special-forms (special-forms)
        subrs (reduce dissoc (subrs) (concat subr-aliases special-forms))]
    (->> (map #(vector (key %) (assoc (val %) :namespace %2))
              subrs
              (find-files-for-tags (map name (keys subrs))))
         (into {})
         (group-by (comp :namespace val)))))

(def illegal-symbols {(symbol "1+") (symbol "1+")
                      (symbol "1-") (symbol "1-")
                      (symbol "/=") 'slash-equals
                      '... '(&rest args)
                      'ARGS... '(&rest args)
                      'ARGUMENTS... '(&rest arguments)})

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
                        " " (pr-str (->> (replace illegal-symbols args)
                                         flatten
                                         (map (comp symbol string/lower-case))))))
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
(ns deuce.scaffold
  (:use [clojure.set :only (intersection)])
  (:require [clojure.java.shell :as sh]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint])
  (:gen-class))

(defmacro eval-in-emacs [& emacs-lisp]
  `(let [env# (zipmap '~(keys &env) ~(vec (keys &env)))
         code# (str "(progn " (apply str (walk/postwalk-replace env# '~emacs-lisp)) ")")]
     (:out (sh/sh "emacs/src/emacs" "-Q" "-batch" "--eval" code#))))

(def symbol-delimiter "--- DEUCE DELIMITER ---")

(defn read-subrs []
  (let [symbol-delimiter symbol-delimiter]
    (eval-in-emacs
     (require 'cl)

     (do-symbols (sym)
                 (when
                     (and (fboundp sym) (subrp (symbol-function sym)))
                   (princ (describe-function sym))
                   (princ symbol-delimiter))))))

(defn read-vars []
  (let [symbol-delimiter symbol-delimiter]
       (eval-in-emacs
        (require 'cl)

        (do-symbols (sym)
                    (when (and (boundp sym)
                               (eq (find-lisp-object-file-name sym 'defvar) 'C-source))
                      (princ (replace-regexp-in-string symbol-delimiter "--- REPLACED DELIMITER ---" (describe-variable sym)))
                      (princ symbol-delimiter))))))

(defn special-forms []
  (->> (string/split (read-subrs) (re-pattern symbol-delimiter))
       (map #(re-find #"(\S+) is a special form in `C source code'" %))
       (remove nil?)
       (map (comp symbol second))
       set))

(defn subrs []
  (->> (string/split (read-subrs) (re-pattern symbol-delimiter))
       (remove empty?)
       (map #(let [[decl & doc] (->> (string/split % #"\n\n")
                                     (drop-while (complement (partial re-find #"\(.*"))))
                   [name & args] (string/split (subs decl 1 (dec (count decl))) #"\s+")]
               [(symbol name) {:args (map symbol args) :doc (string/join "\n\n" doc)}]))
       (into {})))

(defn vars []
  (->> (string/split (read-vars) (re-pattern symbol-delimiter))
       (remove empty?)
       (map #(let [[decl doc] (string/split % #"Documentation:\n")
                   [doc & _] (string/split doc #"\n\nValue:")
                   [name & _] (string/split decl #"\s+")]
               [(symbol name) {:doc doc}]))
       (into {})))

(defn find-files-for-tags [tags]
  (sh/sh "./collect-tags")
  (let [tags (vec tags)]
    (->> (eval-in-emacs
          (setq tags-file-name "emacs/src/TAGS-TEMACS")
          (princ (mapcar (lambda (x) (buffer-name (find-tag-noselect x))) tags)))
         read-string
         (map #(string/replace % #"\..+$" "")))))

(def subr-aliases '#{search-forward-regexp search-backward-regexp internal-temp-output-buffer-show})

(def ns-overrides '{+ "data" / "data" * "data" = "data"})

(defn generate-fn-stubs [subrs]
  (->> (map #(vector (key %) (assoc (val %) :namespace (get ns-overrides (key %) %2)))
            subrs
            (find-files-for-tags (map name (keys subrs))))
       (into {})
       (group-by (comp :namespace val))))

(def illegal-symbols {(symbol "1+") (symbol "1+")
                      (symbol "1-") (symbol "1-")
                      (symbol "/=") 'slash-equals
                      '... '(&rest args)
                      'ARGS... '(&rest args)
                      'ARGUMENTS... '(&rest arguments)})

(defn print-fn-stubs [namespace fns vars]
  (pprint/pprint (list 'ns namespace
                       (list :use ['deuce.emacs-lisp :only '(defun defvar)])
                       (list :require ['clojure.core :as 'c])
                       (list :refer-clojure :exclude
                             (vec (intersection (set (keys (ns-publics 'clojure.core)))
                                                (set (keys fns)))))))
  (doseq [[v {:keys [doc]}] vars]
    (println)
    (println (str "(defvar " v " nil"))
    (when doc
      (print  "  \"")
      (print (-> doc
                 string/trimr
                 (string/replace #"\n" "\n  ")
                 (string/escape {\" "\\\"" \\ "\\\\"})))
      (println "\")")))

  (doseq [[f {:keys [args doc]}] fns]
    (println)
    (println (str "(defun " (if (illegal-symbols f) (str "(clojure.core/symbol \"" (str (illegal-symbols f)) "\")") f)
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
    (println "  )")))

(defn print-special-forms []
  (print-fn-stubs 'deuce.emacs-lisp (select-keys (subrs) (special-forms))))

(defn write-fn-stubs []
  (.mkdir (io/file "src/deuce/emacs"))
  (let [vars (generate-fn-stubs (vars))]
    (doseq [[original fns] (generate-fn-stubs (reduce dissoc (subrs) (concat subr-aliases (special-forms))))
            :let [namespace (symbol (str "deuce.emacs." original))
                  vars (vars original)]]
      (println namespace (str "(" (count fns) " subrs)"))
      (with-open [w (io/writer (io/file "src/deuce/emacs" (str original ".clj")))]
        (binding [*out* w]
          (print-fn-stubs namespace fns vars)))
      (require namespace))))

(defn -main []
  (write-fn-stubs)
  (System/exit 0))
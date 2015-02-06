(ns deuce.emacs-lisp.printer
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [fipp.edn :as fp-edn]
            [fipp.clojure :as fp]
            [deuce.emacs-lisp :as el]))

(def ^:dynamic *pretty-style* :el)

(extend-protocol fp/IPretty
  java.lang.Object
  (-pretty [x ctx]
    (binding [*print-dup* true]
      [:text (pr-str x)]))

  clojure.lang.ISeq
  (-pretty [s ctx]
    (if-let [pretty-special (get (:symbols ctx) (first s))]
      (pretty-special s ctx)
      (fp/list-group [:align (if (symbol? (first s)) 1 0) (interpose :line (map #(fp/pretty % ctx) s))]))))

(extend-protocol fp-edn/IPretty
  java.lang.Object
  (-pretty [x ctx]
    (binding [*print-dup* true]
      [:text (pr-str x)])))

(defn ^:private pretty-docstring [docstring ctx]
  (if (string? docstring)
    [:group
     (concat ["  \""]
             (interpose :break (map #(let [[t s] (fp/-pretty % ctx)]
                                       [:text (subs s 1 (dec (count s)))])
                                    (s/split docstring #"\n")))
             ["\""])]
    [(fp/pretty docstring ctx)]))

(defn ^:private pretty-defun [[head fn-name params & more] ctx]
  (let [[docstring body] (fp/maybe-a string? more)]
    (fp/list-group
     (fp/-pretty head ctx) " " (fp/pretty fn-name ctx) " " (fp/-pretty params (dissoc ctx :symbols))
     (when docstring [:group :break (pretty-docstring docstring ctx)])
     (when (or docstring (seq body)) :break)
     (fp/block (map #(fp/pretty % ctx) body)))))

(defn ^:private pretty-defvar [[head symbol & [initvalue docstring]] ctx]
  (fp/list-group
   (fp/-pretty head ctx) " " (fp/pretty symbol ctx) :line (fp/block [(fp/-pretty initvalue ctx)])
   (when docstring [:group :break (pretty-docstring docstring ctx)])))

(defn ^:private pretty-let [[head varlist & body :as form] ctx]
  (let [varlist (for [kv varlist]
                  (if-let [[k v] (and (seq? kv) kv)]
                    [:span "(" (fp/-pretty k (dissoc ctx :symbols)) " " [:align (fp/pretty v ctx)] ")"]
                    [:span (fp/-pretty kv (dissoc ctx :symbols))]))]
    (fp/list-group
     (fp/-pretty head ctx) " "
     [:group "(" [:align (interpose :break varlist)] ")"]
     (when (seq body) :break)
     (fp/block (map #(fp/pretty % ctx) body)))))

(defn ^:private pretty-cond [[head & clauses] ctx]
  (let [clauses (for [c clauses]
                  [:group (concat [:span "(" (fp/-pretty (first c) ctx)]
                                  (when (> (count c) 1)
                                    [:span :line [:nest 1 (interpose :line (map #(fp/pretty % ctx) (rest c)))]])
                                  [")"])])]
    (fp/list-group
     (fp/-pretty head ctx) " "
     [:align (interpose :break clauses)])))

(defn ^:private pretty-if [[head cond then & else] ctx]
  (fp/list-group
   (fp/-pretty head ctx) " " (fp/pretty cond ctx) :line
   (fp/block [(fp/pretty then ctx)]) (when (seq else) :line)
   (fp/block (map #(fp/pretty % ctx) else))))

(defn ^:private pretty-block [break [head stmnt & block] ctx]
  (fp/list-group
   (fp/-pretty head ctx) " " (fp/pretty stmnt ctx) (when (seq block) break)
   (fp/block (map #(fp/pretty % ctx) block))))

(defn ^:private pretty-lambda [[head args & block] ctx]
  (fp/list-group
   (fp/-pretty head ctx) " " (fp/pretty args (dissoc ctx :symbols)) (when (seq block) :line)
   (fp/block (map #(fp/pretty % ctx) block))))

(defn ^:private pretty-quote [[macro arg] ctx]
  [:span "'" (fp-edn/pretty arg (dissoc ctx :symbols))])

(def ^:private el-symbols
  (fp/build-symbol-map
   {pretty-defun '[defmacro defun]
    pretty-defvar '[defvar defconst]
    pretty-let '[let let*]
    pretty-if '[deuce.emacs-lisp/if if]
    (partial pretty-block :line) '[setq set and or not]
    (partial pretty-block :break) '[while when unless dotimes dolist]
    pretty-lambda '[lambda closure]
    pretty-cond '[cond]
    pretty-quote '[quote]
    fp/pretty-ns '[ns]
    fp-edn/pretty '[#el/sym "\\`"]}))

(defn pprint-el
  ([form] (pprint-el form {}))
  ([form options]
   (fp/pprint form (merge {:symbols el-symbols :width 100} options))))

(defn write-clojure [el-form clj-file]
  (io/make-parents clj-file)
  (binding [*out* (io/writer clj-file)]
    (doseq [form (concat '[(ns deuce.emacs (:refer-clojure :only []))]
                         (map el/el->clj el-form))]
      (case *pretty-style*
        :edn (fp-edn/pprint form)
        :el (pprint-el form)
        (pr form))
      (println))
    (flush)))

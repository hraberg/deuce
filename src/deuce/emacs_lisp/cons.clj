(ns deuce.emacs-lisp.cons
  (:require [clojure.core :as c])
  (:refer-clojure :exclude [list cons])
  (:import [clojure.lang Seqable Sequential
            IPersistentCollection ISeq Cons
            IPersistentList PersistentList LazySeq]
           [java.io Writer]
           [java.lang.reflect Field]))

(defprotocol IList
  (car [this])
  (cdr [this]))

(defprotocol ICons
  (setcar [this val])
  (setcdr [this val]))

(extend-type nil
  IList
  (car [this] nil)
  (cdr [this] nil))

(def ^:private array-class (Class/forName "[Ljava.lang.Object;"))

;; (satisfies? IList object) is slow - not sure if I trust this old comment..
(defn listp [object]
  (or (nil? object) (and (sequential? object)
                         (not= array-class (type object)))))

(defn consp [object]
  (instance? PersistentList object))

(defn dotted-list? [x]
  (and (seq? x) (= '. (last (butlast x)))
       (listp (last x))))

(defn dotted-list-ending-in-pair? [x]
  (and (seq? x) (= '. (last (butlast x)))
       (not (listp (last x)))))

(defn dotted-pair? [x]
  (and (seq? x) (= 3 (count x)) (= '. (last (butlast x)))))

(extend-type IPersistentCollection
  IList
  (car [this] (first this))
  (cdr [this]
    (let [cdr (next this)]
      (if (= '. (first cdr))
        (second cdr)
        cdr))))

(defn field-accessor [prefix field class]
  (eval `(def ^{:private true :tag `Field}
           ~(symbol (str prefix (name field))) (doto (.getDeclaredField ~class ~(name field))
                                              (.setAccessible true)))))

(doseq [field '[_first _rest _count]]
  (field-accessor "l" field PersistentList))

(declare list)

(extend-type PersistentList
  ICons
  (setcar [^PersistentList this val]
    (do (.set l_first this val)
        val))
  (setcdr [^PersistentList this val]
    (if (or (instance? IPersistentList val) (nil? val) (= () val))
      (do
        (.set l_rest this val)
        (.set l_count this (int (inc (count val))))) ;; this gets out of sync when changing part of the tail.
      (if (dotted-pair? this)
        (setcar (rest (rest this)) val)
        (do
          (.set l_rest this (c/list '. val))
          (.set l_count this (int 3)))))
    val))

;; This should really be eval-expression-print-length, maybe move this to deuce.emacs.print
;; There's also eval-expression-print-level which controls nesting.
;; There's also print-level and print-lenght, seem to be nil in Emacs.
(def ^:private max-print-length 12)

(defn ellipsis [coll]
  (let [s (seq coll)]
    (seq (concat (doall (take max-print-length s))
                 (when (< max-print-length (count s))
                   ['...])))))

(defn print-list [c ^Writer w]
  (.write w "(")
  (loop [c c idx 1]
    (if (> idx max-print-length)
      (.write w "...)")
      (do
        (.write w (pr-str (car c)))
        (cond
         (not (listp (cdr c))) (.write w (str " . " (pr-str (cdr c)) ")"))
         (seq (cdr c)) (do
                         (.write w " ")
                         (recur (cdr c) (inc idx)))
         :else (.write w ")"))))))

;; (defmethod print-method PersistentList [c ^Writer w]
;;   (print-list c w))

;; (defmethod print-method Cons [c ^Writer w]
;;   (print-list c w))

(defn pair [car cdr]
  (if (listp cdr)
    (doto (c/list car)
      (setcdr cdr))
    (c/list car '. cdr)))

;; Fix uses of (apply cons/list ...) to something saner
(defn list [& objects]
  (when (seq objects)
    (pair (car objects)
          (apply list (cdr objects)))))

(defn last-cons [l]
  (if (not (consp (cdr l))) l (recur (cdr l))))

;; Figure out where this is actually needed.
(defn maybe-seq [x]
  (if (and (seq? x)
;           (not (dotted-pair? x))
           (not (consp x)))
    (if (dotted-list-ending-in-pair? x)
      (apply c/list x)
      (apply list x))
    x))

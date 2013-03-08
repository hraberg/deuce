(ns deuce.emacs-lisp.cons
  (:refer-clojure :exclude [list cons])
  (:import [clojure.lang Seqable Sequential
            IPersistentCollection ISeq Cons
            IPersistentList PersistentList]
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

(defn dotted-pair? [x]
  (and (seq? x) (= '. (last (butlast x)))))

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
        (.set l_count this (int (inc (count val)))))
      (if (dotted-pair? this)
        (setcar (rest (rest this)) val)
        (do
          (.set l_rest this (clojure.core/list '. val))
          (.set l_count this (int 3)))))
    val))

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
         (not (satisfies? IList (cdr c))) (.write w (str " . " (pr-str (cdr c)) ")"))
         (cdr c) (do
                   (.write w " ")
                   (recur (cdr c) (inc idx)))
         :else (.write w ")"))))))

(defmethod print-method PersistentList [c ^Writer w]
  (print-list c w))

(defmethod print-method Cons [c ^Writer w]
  (print-list c w))

(defn pair [car cdr]
  (if (satisfies? IList cdr)
    (let [l (clojure.core/list car)]
      (setcdr l cdr)
      l)
    (clojure.core/list car '. cdr)))

;; Fix uses of (apply cons/list ...) to something saner
(defn list [& objects]
  (when (seq objects)
    (pair (car objects)
          (apply list (cdr objects)))))

(defn last-cons [l]
  (if (not (satisfies? ICons (cdr l))) l (recur (cdr l))))

(defn cons-expand [form]
  (let [c (apply list (drop-last 2 form))]
    (setcdr (last-cons c) (last form))
    c))

;; Figure out where this is actually needed.
(defn maybe-seq [x]
  (if (and (seq? x)
           (not (instance? PersistentList x)))
    (if (dotted-pair? x)
      x
;      (cons-expand x)
      (apply list x))
    x))

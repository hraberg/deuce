(ns deuce.emacs-lisp.cons
  (:refer-clojure :exclude [list cons])
  (:import [clojure.lang Seqable Sequential IPersistentCollection]
           [java.io Writer]
           [deuce.emacs_lisp Cons]))

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

(extend-type IPersistentCollection
  IList
  (car [this] (first this))
  (cdr [this] (next this)))

(def ^:private max-print-length 12)

(defn ellipsis [coll]
  (let [s (seq coll)]
    (seq (concat (doall (take max-print-length s))
                 (when (< max-print-length (count s))
                   ['...])))))

(extend-type Cons
  IList
  (car [this] (.fst this))
  (cdr [this] (.rst this))

  ICons
  (setcar [this val] (set! (.fst this) val))
  (setcdr [this val] (set! (.rst this) val)))

(defmethod print-method Cons [c ^Writer w]
  (.write w "(")
  (loop [c c idx 1]
    (if (> idx max-print-length)
      (.write w "...)")
      (do
        (.write w (str (.fst c)))
        (cond
         (not (satisfies? IList (.rst c))) (.write w (str " . " (.rst c) ")"))
         (.rst c) (do
                    (.write w " ")
                    (recur (.rst c) (inc idx)))
         :else (.write w ")"))))))

(defmethod print-dup Cons [c ^Writer out]
  (.write out (str "#deuce/cons (" (pr-str (.fst c)) " . " (pr-str (.rst c)) ")")))

(defn pair [car cdr]
  (Cons. car cdr))

(defn list [& objects]
  (when (seq objects)
    (pair (first objects)
          (apply list (rest objects)))))

(defn last-cons [l]
  (if (not (satisfies? ICons (cdr l))) l (recur (cdr l))))

(defn maybe-seq [x]
  (if (seq? x) (apply list x) x))

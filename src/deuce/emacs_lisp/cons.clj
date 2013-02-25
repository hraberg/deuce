(ns deuce.emacs-lisp.cons
  (:refer-clojure :exclude [list])
  (:import [clojure.lang Seqable Sequential IPersistentCollection]))

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

(deftype Cons [^:volatile-mutable fst ^:volatile-mutable rst]
  Sequential

  IList
  (car [this] fst)
  (cdr [this] rst)

  ICons
  (setcar [this val] (set! fst val))
  (setcdr [this val] (set! rst val))

  Seqable
  (seq [this]
    (cons fst
          (if (satisfies? IList rst)
            (seq rst)
            [rst])))

  Object
  (toString [this]
    (if (not (satisfies? IList rst))
      (str "(" fst " . " rst ")")
      (str (ellipsis this))))
  (equals [this that]
    (cond (identical? this that) true
          (satisfies? IList that) (and
                                   (= fst (car that))
                                   (= rst (cdr that)))
          (instance? Sequential that) (= (seq this) that)
          :else false))
  (hashCode [_]
    (+ (* 31 (if (nil? fst) 0 (.hashCode fst)))
       (if (nil? rst) 0 (.hashCode rst)))))

(defmethod print-method Cons [c w]
  (.write w (str c)))

(defmethod print-dup Cons [c out]
  (.write out (str "#=" `(deuce.util.Cons. ~(str (car c)) ~(str (cdr c))))))

(defn list [& objects]
  (when (seq objects)
    (Cons. (first objects)
           (apply list (rest objects)))))

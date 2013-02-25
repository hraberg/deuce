(ns deuce.util
  (:refer-clojure :exclude [list])
  (:import [clojure.lang Seqable]))

(defprotocol IList
  (car [this])
  (cdr [this]))

(extend-type nil
  IList
  (car [this] nil)
  (cdr [this] nil))

(defprotocol ICons
  (setcar [this val])
  (setcdr [this val]))

(deftype Cons [^:volatile-mutable fst ^:volatile-mutable rst]
  IList
  (car [this] fst)
  (cdr [this] rst)

  ICons
  (setcar [this val] (set! fst val))
  (setcdr [this val] (set! rst val))

  Seqable
  (seq [this]
    (cons fst
          (cond (= fst nil)
                nil
                (satisfies? ICons rst)
                (seq rst)
                :else
                rst)))

  Object
  (equals [this that]
    (or (identical? this that)
        (and (satisfies? IList that)
             (= fst (car that))
             (= rst (cdr that)))))
  (hashCode [_]
    (+ (* 31 (if (nil? fst) 0 (.hashCode fst)))
       (if (nil? rst) 0 (.hashCode rst)))))


(def ^:private max-print-length 12)

(defn ellipsis [seq]
  (concat (doall (take max-print-length seq))
          (when (< max-print-length (count seq))
            ['...])))

(defmethod print-method Cons [c w]
  (if (not (satisfies? IList (cdr c)))
    (.write w (str "(" (car c) " . " (cdr c) ")"))
    (print-method (ellipsis (seq c)) w)))

(defmethod print-dup Cons [c out]
  (.write out (str "#=" `(deuce.util.Cons. ~(str (car c)) ~(str (cdr c))))))

(defn list [& objects]
  (when (seq objects)
    (Cons. (first objects)
           (apply list (rest objects)))))

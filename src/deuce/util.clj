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

(defmethod print-method Cons [c w]
  (loop [s (str "(" (car c))
         c c]
    (cond (not (satisfies? IList (cdr c)))
          (.write w (str s " . " (cdr c) ")"))

          (nil? (cdr c))
          (.write w (str s ")"))

          (> (count s) 20)
          (.write w (str s "...)"))

          :else
          (recur (str s " " (car (cdr c)))
                 (cdr c)))))

(defmethod print-dup Cons [c out]
  (.write out (str "#=" `(deuce.util.Cons. ~(str (car c)) ~(str (cdr c))))))

(defn list [& objects]
  (when (seq objects)
    (Cons. (first objects)
           (apply list (rest objects)))))

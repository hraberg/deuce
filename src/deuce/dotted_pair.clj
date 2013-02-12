(ns deuce.dotted-pair)

(defprotocol IDottedPair
  (car [this])
  (set-car! [this val])
  (cdr [this])
  (set-cdr! [this val]))

(deftype DottedPair [^:volatile-mutable fst ^:volatile-mutable snd]
  IDottedPair
  (car [this] fst)
  (set-car! [this val] (set! fst val))
  (cdr [this] snd)
  (set-cdr! [this val] (set! snd val))

  Object
  (equals [this that]
    (or (identical? this that)
        (and (instance? DottedPair that)
             (= fst (car that))
             (= snd (cdr that)))))
  (hashCode [_]
    (+ (* 31 (if (nil? fst) 0 (.hashCode fst)))
       (if (nil? snd) 0 (.hashCode snd)))))

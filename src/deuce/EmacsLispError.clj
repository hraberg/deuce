(ns deuce.EmacsLispError
  (:gen-class :extends RuntimeException
              :implements [clojure.lang.IDeref]
              :init init
              :constructors {[Object] []
                             [clojure.lang.Symbol Object] []}
              :state state
              :methods [[symbol [] clojure.lang.Symbol]
                        [data [] Object]]))

(defn -init
  "Initializes the EmacsLispError object with a symbol and object."
  ([data]
     [[] {:symbol nil :data data}])
  ([symbol data]
     [[] {:symbol symbol :data data}]))

(defn -deref
  "Deref returns the state of the object."
  [this]
  (.state this))

(defn -getMessage
  "Returns the error message."
  [this]
  (if (nil? (:data @this))
    (format "Lisp error: [no-catch %s %s]" symbol (:data @this))
    (format "Lisp error: (%s)" (:data @this))))

(defn -toString
  "Returns a string representation of the error."
  [this]
  (.getMessage this))

(defn -data
  "Returns the data object in the error."
  [this]
  (:data @this))

(defn -symbol
  "Returns the symbol in the error."
  [this]
  (:symbol @this))

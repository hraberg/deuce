(ns deuce.EmacsLispError
  (:gen-class :extends RuntimeException
              :init init
              :constructors {[Object] []
                             [Object clojure.lang.Symbol] []}
              :state state))

(defn -init [data & [symbol]]
  [[] {:data data :symbol symbol}])

(defn -getMessage [this]
  (if-let [data (:data (.state this))]
    (format "Lisp error: (%s)" data)
    (format "Lisp error: [no-catch %s nil]" symbol)))

(defn -toString [this]
  (.getMessage this))

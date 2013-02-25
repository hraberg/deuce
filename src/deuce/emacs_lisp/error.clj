(ns deuce.emacs-lisp.error
  (:gen-class :extends RuntimeException
              :init init
              :constructors {[Object] []
                             [Object clojure.lang.Symbol] []}
              :state state))

(defn -init [data symbol]
  [[] {:data data :symbol symbol}])

(defn -getMessage [this]
  (str (cons (:symbol (.state this)) (:data (.state this)))))

(defn -toString [this]
  (.getMessage this))

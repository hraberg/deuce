(ns deuce.emacs-lisp.parser
  (import [java.util Scanner]
          [java.io StringReader StreamTokenizer]
          [java.util.regex Pattern]))

(declare tokenize)

(def ^:private ^Pattern re-str #"(?s)([^\"\\]*(?:\\.[^\"\\]*)*)\"")

(defn ^:private tokenize-all [^Scanner sc]
  (take-while identity (repeatedly (partial tokenize sc))))

(defn ^:private tokenize [^Scanner sc]
  (let [find (fn [^Pattern re h] (.findWithinHorizon sc re (int h)))]
    (condp find 1
      #"\s" (recur sc)
      #"[)\]]" nil
      #"\(" (apply list (tokenize-all sc))
      #"\[" (list 'quote (vec (tokenize-all sc)))
      #"," (list (if (find #"@" 1) (symbol "\\,@") (symbol "\\,")) (tokenize sc))
      #"'" (list 'quote (tokenize sc))
      #"`" (let [form (tokenize sc)] (if (symbol? form)
                                       (list 'quote form)
                                       (list (symbol "\\`") form)))
      #":" (keyword (.next sc))
      #"\." (list 'quote (symbol "."))
      #"\?" (list 'quote (symbol (str \? (.next sc))))
      #"\"" (.sval (doto (StreamTokenizer. (StringReader. (str \" (find re-str 0))))
                     (.nextToken)))
      #";" (list 'clojure.core/comment (.nextLine sc))
      #"#" (condp find 1
             #"'" (list 'var (tokenize sc))
             #"\(" (let [[object start end properties] (tokenize-all sc)]
                     (list 'set-text-properties start end properties object))
             #"x" (.nextInt sc (int 16))
             #"o" (.nextInt sc (int 8))
             #"b" (.nextInt sc (int 2))
             (when (.hasNext sc #"\d+r\S+")
               (let [radix (find #"\d+" 0)]
                 (find #"r" 1)
                 (.nextInt sc (Integer/parseInt radix)))))
      (cond
       (.hasNextLong sc) (.nextLong sc)
       (.hasNextDouble sc) (.nextDouble sc)
       (.hasNext sc) (symbol (.replaceAll (.next sc) "/" "_SLASH_"))))))

(defn parse [r]
  (tokenize-all (doto (if (string? r) (Scanner. r) (Scanner. r "UTF-8"))
                  (.useDelimiter #"(\s+|\]|\)|\"|;)"))))

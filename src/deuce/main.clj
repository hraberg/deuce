(ns deuce.main
  (require [deuce.emacs]
           [deuce.emacs-lisp :as el]
           [deuce.emacs.data :as data]
           [deuce.emacs.eval :as eval]
           [deuce.emacs.editfns :as editfns]
           [deuce.emacs.lread :as lread])
  (import [java.util Stack])
  (:gen-class))

(el/setq command-line-args ["deuce"])

(defn deuce-loadup []
  (el/setq before-init-time (editfns/current-time))
  (lread/load "deuce-loadup.el")
  (el/setq after-init-time (editfns/current-time)))

(defn -main [& args]
  (when-not (some #{"-batch" "--batch"} args)
    (println "Batch mode required, run with -batch or --batch")
    (System/exit 1))

  (el/setq command-line-args (cons "deuce" args))

  (let [args (doto (Stack.)
               (.addAll (reverse args)))
        pop (fn [opt]
              (if (empty? args)
                (do
                  (printf ("Option `%s' requires an argument\n" opt))
                  (System/exit 1))
                (.pop args)))
        option #(hash-set (str "-" %) (str "--" %))]

    (while (seq args)
      (let [opt (.pop args)]
        (condp some [opt]
          #{"--eval" "--execute"} (do
                                    (deuce-loadup)
                                    (eval/eval (deuce.emacs.lread/read (pop opt))))
          (option "script") (do
                              (deuce-loadup)
                              (lread/load (pop opt)))
          (option "version") (do (printf "GNU Emacs %s\n" (data/symbol-value 'emacs-version))
                                 (printf "%s\n" (data/symbol-value 'emacs-copyright))
                                 (printf "GNU Emacs comes with ABSOLUTELY NO WARRANTY.\n")
                                 (printf "You may redistribute copies of Emacs\n")
                                 (printf "under the terms of the GNU General Public License.\n")
                                 (printf "For more information about these matters, ")
                                 (printf "see the file named COPYING.\n")
                                 (flush)
                                 (System/exit 0))
          (option "batch") nil
          (printf "Unknown option `%s'\n" opt))))))

(ns deuce.main
  (:require [deuce.emacs]
            [deuce.emacs-lisp :as el]
            [deuce.emacs.data :as data]
            [deuce.emacs.eval :as eval]
            [deuce.emacs.editfns :as editfns]
            [deuce.emacs.lread :as lread])
  (:import [java.util Stack])
  (:gen-class))

(defn loadup []
  (el/setq before-init-time (editfns/current-time))
  (lread/load "deuce-loadup.el")
  (el/setq after-init-time (editfns/current-time)))

;; We want to support emacs -nw -q initially. -q is --no-init-file
(defn -main [& args]
  (when-not (some #{"-batch" "--batch"} args)
    (println "Batch mode required, run with -batch or --batch")
    (System/exit 1))
  ;; Alternatively #{"-nw" "--no-window-system"}

  ;; We won't be able to support any random .emacs.d/init.el for quite some time.
  ;; (when-not (some #{"-q" "--no-init-file"} args)
  ;;   (println "Loading of init file not supported, run with -q or --no-init-file")
  ;;   (System/exit 1))

  (el/setq command-line-args (cons "src/bootstrap-emacs" args))

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
          ;; Should be handled by startup.el
          #{"--eval" "--execute"} (do (loadup)
                                      (eval/eval (deuce.emacs.lread/read (pop opt))))
          ;; Should be changed to -scriptload and then handled by startup.el
          (option "script") (do (loadup)
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
          (option "batch") (el/setq noninteractive true)
          #{"-nw" "--no-window-system,"} (el/setq inhibit-window-system true)
          ;; This is not true, startup.el knowns the full set.
          (printf "Unknown option `%s'\n" opt))))
    ;; Pontentially call out and init the clojure-lanterna terminal (when-not inhibit-window-system)
    ;; startup.el may take care of this indirectly and make the callback for us.
    ;; Store the remaining argument stack in command-line-args and call (loadup) which will call (eval top-level)
    ))

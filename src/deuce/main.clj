(ns deuce.main
  (:require [deuce.emacs]
            [deuce.emacs-lisp :as el]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.data :as data]
            [deuce.emacs.eval :as eval]
            [deuce.emacs.editfns :as editfns]
            [deuce.emacs.lread :as lread]
            [taoensso.timbre :as timbre])
  (:gen-class))

;; We want to support emacs -nw -q initially. -q is --no-init-file
(defn -main [& args]
  (let [option #(hash-set (str "-" %) (str "--" %))
        args (map
              #(condp some [%]
                 (option "script") "-scriptload"
                 (option "version") (do (printf "GNU Emacs %s\n" (data/symbol-value 'emacs-version))
                                        (printf "%s\n" (data/symbol-value 'emacs-copyright))
                                        (printf "GNU Emacs comes with ABSOLUTELY NO WARRANTY.\n")
                                        (printf "You may redistribute copies of Emacs\n")
                                        (printf "under the terms of the GNU General Public License.\n")
                                        (printf "For more information about these matters, ")
                                        (printf "see the file named COPYING.\n")
                                        (flush)
                                        (System/exit 0))
                 (option "batch") (do (el/setq noninteractive true) nil)
                 #{"-nw" "--no-window-system,"} (do (el/setq inhibit-window-system true) nil)
                 %) args)]

    (el/setq command-line-args (alloc/cons "src/bootstrap-emacs" (apply alloc/list (remove nil? args))))
    (lread/load "deuce-loadup.el")
    (println)
    (flush)
    ;; Pontentially call out and init the clojure-lanterna terminal (when-not inhibit-window-system)
    ;; startup.el may take care of this indirectly and make the callback for us.
    ))

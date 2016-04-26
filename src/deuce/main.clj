(ns deuce.main
  (:require [clojure.java.io :as io]
            [deuce.emacs]
            [deuce.emacs-lisp :as el]
            [deuce.emacs-lisp.globals :as globals]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.buffer :as buffer]
            [deuce.emacs.data :as data]
            [deuce.emacs.dispnew :as dispnew]
            [deuce.emacs.editfns :as editfns]
            [deuce.emacs.eval :as eval]
            [deuce.emacs.keymap :as keymap]
            [deuce.emacs.keyboard :as keyboard]
            [deuce.emacs.lread :as lread]
            [deuce.emacs.terminal :as terminal]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.core :as timbre-appenders]
            [dynapath.util :as dp])
  (:import [java.io FileNotFoundException InputStreamReader]
           [java.awt Toolkit]
           [java.awt.datatransfer DataFlavor StringSelection]
           [clojure.lang ExceptionInfo])
  (:gen-class))

;; Start Deuce like this:
;;    make run-dev

;; This will put you in *scratch*, with the keyboard enabled.
;; There's no minibuffer yet, so you have to switch buffer from the REPL.
;; Several keyboard commands fail or are a bit off (like move-end-of-line).

;; Connect to nREPL from Emacs on port 7888:
;; user> (in-ns 'deuce.emacs)  ;; We're now in Emacs Lisp

;; Tail the log at ~/.deuce.d/deuce.log
;; Errors are also visible in the Echo Area

(defn nrepl [port]
  (require 'clojure.tools.nrepl.server)
  (with-out-str
    ((resolve 'clojure.tools.nrepl.server/start-server)
     :port port
     :handler (try
                (require 'cider.nrepl)
                (resolve 'cider.nrepl/cider-nrepl-handler)
                (catch Exception _
                  (resolve 'clojure.tools.nrepl.server/default-handler)))))
  (println "nrepl server listening on" port))

(def deuce-dot-d (str (doto (io/file (System/getProperty "user.home") ".deuce.d")
                        .mkdirs)))
(def ^:dynamic *emacs-compile-path* *compile-path*)

(defn init-user-classpath []
  (dp/add-classpath-url (ClassLoader/getSystemClassLoader) (.toURL (io/file deuce-dot-d)))
  (alter-var-root #'*emacs-compile-path* (constantly deuce-dot-d)))

(defn load-user-init-file []
  (let [init-file (io/file deuce-dot-d "init.clj")]
    (try
      (when (.exists init-file)
        (load-file (str init-file)))
      (catch Exception e
        (timbre/error e (format "An error occurred while loading `%s':" (str init-file)))))))

(def running (atom nil))
(def frame-time-ms 15)

(defn running? []
  (true? @running))

;; Not the real thing, but keeps the UI changing while using the REPL before we got a real command loop.
(defn start-render-loop []
  (reset! running true)
  (future
    (dispnew/redraw-display)
    (while (running?)
      (try
        (dispnew/redisplay)
        (Thread/sleep frame-time-ms)
        (catch Exception e
          (reset! running nil)
          (binding [*ns* (the-ns 'clojure.core)]
            (timbre/error e "An error occured during the render loop"))
          (throw e))))
    (reset! running nil)))

(defn start-command-loop []
  (reset! running true)
  (future
    (keyboard/discard-input)
    (while (running?)
      (try
        (let [def (keymap/key-binding (keyboard/read-key-sequence-vector nil))]
          (when (and def (not (keymap/keymapp def)))
            (keyboard/command-execute def)))
        (catch ExceptionInfo e
          (binding [*ns* (the-ns 'clojure.core)]
            (timbre/error (.getMessage e))))
        (catch Exception e
          ;; This is a simplification, but makes you aware of the error without tailing the log.
          ((ns-resolve 'deuce.emacs.keyboard 'echo) (.getMessage e))
          (binding [*ns* (the-ns 'clojure.core)]
            (timbre/error (el/cause e) "An error occured during the input loop")))))))

(defn start-ui []
  (start-render-loop)
  (start-command-loop))

(defn stop-ui []
  (reset! running :stop)
  (while @running
    (Thread/sleep frame-time-ms)))

(defn init-clipboard []
  (let [clipboard (.getSystemClipboard (Toolkit/getDefaultToolkit))]
    (el/setq interprogram-cut-function
             #(let [selection (StringSelection. %)]
                (.setContents clipboard selection selection)))

    (el/setq interprogram-paste-function
             #(.getData clipboard DataFlavor/stringFlavor))))

(defn inside-emacs? []
  (= "dumb" (System/getenv "TERM")))

;; Callback run by faces/tty-run-terminal-initialization based on deuce.emacs.term/tty-type returning "lanterna"
;; Has Emacs Lisp proxy in deuce.emacs.
(defn terminal-init-lanterna []
  (try
    (when-not (inside-emacs?)
      (init-user-classpath)
      ((ns-resolve 'deuce.emacs.terminal 'init-initial-terminal))
      ;; We need to deal with resize later, it queries and gets the result on System/in which we have taken over.

      ;; Initialize the real TERM, should setup input-decode-map and local-function-key-map
      (eval/eval '(tty-run-terminal-initialization (selected-frame)
                                                   (getenv-internal "TERM")))

      (init-clipboard)
      (start-ui))
    (catch Exception e
      (terminal/delete-terminal)
      (timbre/error e "An error occured during Lanterna init")
      (throw e))))

(defn restart []
  (let [args (next (data/symbol-value 'command-line-args))]
    (terminal/delete-terminal)
    (some-> 'deuce.main/-main resolve (apply args))
    (terminal-init-lanterna)
    :ok))

(timbre/merge-config!
 {:appenders
  {:deuce-buffer-appender
   {:min-level :debug :enabled? true :async? true
    :fn (fn [{:keys [output-fn] :as data}]
          (binding [buffer/*current-buffer* (buffer/get-buffer-create "*Deuce*")]
            (editfns/insert (str (output-fn data) \newline))))}
   :println (merge (timbre-appenders/println-appender)
                   {:enabled? (inside-emacs?)})
   :spit (merge (timbre-appenders/spit-appender {:fname (str (io/file deuce-dot-d "deuce.log"))})
                {:min-level :debug :enabled? true})}})

;; We want to support emacs -q initially. -q is --no-init-file
(defn -main [& args]
  (timbre/debug "Starting Deuce")
  (let [option #(hash-set (str "-" %) (str "--" %))
        inhibit-window-system (atom nil)
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
                 (option "nrepl") (nrepl 7888)
                 #{"-nw" "--no-window-system,"} (do (reset! inhibit-window-system true))
                 %) args)]

    (el/setq command-line-args (alloc/cons "src/bootstrap-emacs" (apply alloc/list (remove nil? args))))

    (lread/load "deuce-loadup.el")
    (when (data/symbol-value 'init-file-user)
      (load-user-init-file))

    ;; /* Enter editor command loop.  This never returns.  */
    (keyboard/recursive-edit)))

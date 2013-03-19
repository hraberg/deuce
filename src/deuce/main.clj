(ns deuce.main
  (:require [clojure.string :as s]
            [lanterna.screen :as sc]
            [deuce.emacs]
            [deuce.emacs-lisp :as el]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.buffer :as buffer]
            [deuce.emacs.data :as data]
            [deuce.emacs.editfns :as editfns]
            [deuce.emacs.eval :as eval]
            [deuce.emacs.fns :as fns]
            [deuce.emacs.frame :as frame]
            [deuce.emacs.lread :as lread]
            [deuce.emacs.terminal :as terminal]
            [deuce.emacs.window :as window]
            [deuce.emacs.xdisp :as xdisp]
            [taoensso.timbre :as timbre])
  (:gen-class))

(defn swank [port]
  (require 'swank.swank)
  (with-out-str
    ((resolve 'swank.swank/start-repl) port))
  (println "Swank connection opened on" port))

(defn nrepl [port]
  (require 'clojure.tools.nrepl.server)
  (with-out-str
    ((resolve 'clojure.tools.nrepl.server/start-server) :port port))
  (println "nrepl server listening on" port))

(defn render-mode-line []
  (xdisp/format-mode-line (buffer/buffer-local-value 'mode-line-format (buffer/current-buffer))))

;; The way this does this is probably utterly wrong, written by data inspection, not reading Emacs source.
;; But produces the expected result:
(defn render-menu-bar []
  (when (data/symbol-value 'menu-bar-mode)
    (let [map-for-mode #(let [map (symbol (str % "-map"))]
                          (when (data/boundp map)
                            (data/symbol-value map)))
          menu-name #(last %)
          ;; The consp check here is suspicious.
          ;; There's a "menu-bar" string in there which probably shouldn't be.
          menus-for-map #(map menu-name (filter data/consp (fns/nthcdr 2 (fns/assq 'menu-bar %))))
          menu-bar-by-name #(data/symbol-value (symbol (str "menu-bar-" %)))
          global-map (data/symbol-value 'global-map)
          major-mode-map (map-for-mode (data/symbol-value 'major-mode))
          minor-mode-maps (map map-for-mode (data/symbol-value 'minor-mode-list))
          final-items (map menu-bar-by-name (data/symbol-value 'menu-bar-final-items))
          menu-bar (concat (mapcat menus-for-map (concat [global-map major-mode-map] minor-mode-maps))
                           (map menu-name final-items))]
      (st/join " " menu-bar))))

;; Renders a single window using Lanterna. Scrolling and position of point are very buggy.
;; Hard to bootstrap, requires fiddling when connected to Swank inside Deuce atm.
(defn display-using-lanterna []
  (def colors {:fg :black :bg :default})
  (def reverse-video {:fg :white :bg :black})
  (declare screen)

  (defn puts
    ([x y s] (puts x y s colors))
    ([x y s opts] (sc/put-string screen x y (str s) opts)))

  (defn line
    ([y] (line y colors))
    ([y opts]
       (let [[width _] (sc/get-size screen)]
         (puts 0 y (apply str (repeat width " ")) opts))))

  (defn blank [_ height]
    (sc/clear screen)
    (sc/redraw screen)
    (doseq [y (range 0 height)]
      (line y)))

  (def screen (terminal/frame-terminal))
  (let [[width height] (sc/get-size screen)
        window (window/selected-window)
        buffer (window/window-buffer window)
        mode-line (- height 2)]
    (reset! (.total-cols window) width)
    (reset! (.total-lines window) (- mode-line 2))

    ;; Menu Bar
    (line 0 reverse-video)
    (puts 0 0 (render-menu-bar) reverse-video)

    ;; Point
    (let [line-indexes ((ns-resolve 'deuce.emacs.cmds 'line-indexes)
                        (str (.beg (.own-text buffer))))
          pt @(.pt buffer)
          line ((ns-resolve 'deuce.emacs.cmds 'pos-to-line) line-indexes pt)
          scroll (max (- line @(.total-lines window)) 0)
          px (dec (- pt (aget line-indexes line)))
          py (inc (- line scroll))
          [px py] (if (neg? px) ;; Horrible.
                    [(dec (- pt (aget line-indexes (dec line)))) (dec py)]
                    [px py])]
      (sc/move-cursor screen (max px 0) py)

      ;; Window Text
      (let [lines (s/split (.beg (.own-text buffer)) #"\n")
            buffer-start 1]
        (dotimes [n (max (count lines) @(.total-lines window))]
          (when (< (+ scroll n) (count lines))
            (puts 0 (+ buffer-start n) (format (str "%-" width "s")
                                               (nth lines (+ scroll n))))))))

    ;; Mode Line
    (line mode-line reverse-video)
    (puts 0 mode-line (render-mode-line) reverse-video)

    ;; Mini Buffer
    (let [mini-buffer (dec height)]
      (puts 0 mini-buffer
            (format (str "%-" width "s")
                    (.beg (.own-text (window/window-buffer
                                      (window/minibuffer-window)))))))
    (sc/redraw screen)))

;; Doesn't take window size and scrolling into account.
(defn display-visible-state-of-emacs []
  (println (render-menu-bar))
  (doseq [window (window/window-list nil true)
          :let [buffer (window/window-buffer window)]]
    (when-let [header-line (buffer/buffer-local-value 'header-line-format buffer)]
      (println (xdisp/format-mode-line header-line nil window buffer)))
    (let [s (str (.beg (.own-text buffer)))
          point (dec @(.pt buffer))]
      (if (= window (window/selected-window))
        (println (str (subs s 0 point) \u258b (let [after-point (if (= (editfns/char-after) \newline)
                                                                  point
                                                                  (inc point))]
                                                (when (< after-point (count s))
                                                  (subs s after-point (count s))))))
        (println s)))
    (when-let [mode-line (and (not (re-find #" \*" (buffer/buffer-name buffer)))
                              (buffer/buffer-local-value 'mode-line-format buffer))]
      (println (xdisp/format-mode-line mode-line nil window buffer)))))

(defn display-state-of-emacs []
  (doseq [frame (frame/frame-list)]
    (println "---------------" frame
             (if (= frame (frame/selected-frame)) "--- [selected frame]" ""))
    (println (render-menu-bar)))
  (doseq [window (window/window-list nil true)
          :let [buffer (window/window-buffer window)]]
    (println "---------------" window
             (if (= window (window/selected-window)) "--- [selected window]" ""))
    (when-let [header-line (buffer/buffer-local-value 'header-line-format buffer)]
      (println (xdisp/format-mode-line header-line nil window buffer)))
    (when-let [mode-line (and (not (re-find #" \*" (buffer/buffer-name buffer)))
                              (buffer/buffer-local-value 'mode-line-format buffer))]
      (println (xdisp/format-mode-line mode-line nil window buffer))))
  (doseq [buffer (buffer/buffer-list)
          :let [name (buffer/buffer-name buffer)
                messages? (= name "*Messages*")]]
    (println "---------------" buffer
             (cond
              (= name (buffer/buffer-name)) "--- [current buffer]"
              messages? "--- [see stdout above]"
              :else ""))
    (when-not messages?
      (println (str (.beg (.own-text buffer)))))))

;; We want to support emacs -q initially. -q is --no-init-file
(defn -main [& args]
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
                 (option "swank-clojure") (swank 4005)
                 (option "nrepl") (nrepl 7888)
                 #{"-nw" "--no-window-system,"} (do (reset! inhibit-window-system true))
                 %) args)]

    (el/setq command-line-args (alloc/cons "src/bootstrap-emacs" (apply alloc/list (remove nil? args))))

    (when-not (data/symbol-value 'noninteractive)
      ;; Emacs opens the terminal before loadup.
      ;; In temacs you get an empty frame, with the actual load echoed to the Echo Area.
      ;; The mode line is the default (just dashes), the cursor is top left, and there's no menu bar.
      ;; This means we are a bit too eager in deuce.emacs setting up the *scratch* buffer.
      ;; inhibit-window-system could maybe be used to switch between :text and :swing in lanterna.
      )

    (lread/load "deuce-loadup.el")
    ;; Dump the current buffers etc. to stdout until we have display. *Messages* is already echoed to stdout.
    (display-state-of-emacs)

    ;; Pontentially call out and init the clojure-lanterna terminal (when-not inhibit-window-system)
    ;; startup.el may take care of this indirectly and make the callback for us.
    ))

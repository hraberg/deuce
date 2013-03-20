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

(defn render-mode-line
  [] (xdisp/format-mode-line (buffer/buffer-local-value 'mode-line-format (buffer/current-buffer))))

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

  ;; If the screen gets messed up by other output like a stack trace you need to call this.
  (defn blank
    ([] (apply blank (sc/get-size screen)))
    ([_ height]
       (sc/clear screen)
       (sc/redraw screen)
       (doseq [y (range 0 height)]
         (line y))))

  (def screen (terminal/frame-terminal))
  (let [[width height] (sc/get-size screen)
        mini-buffer-window (window/minibuffer-window)
        mini-buffer (- height (window/window-total-height mini-buffer-window))
        menu-bar-mode (data/symbol-value 'menu-bar-mode)
        menu-bar (if menu-bar-mode 1 0)
        pad (fn [s cols] (format (str "%-" cols "s") s))]

    ;; Menu Bar
    (when menu-bar-mode
      (puts 0 0 (pad (render-menu-bar) width) reverse-video))

    ;; This should walk (frame-root-window), not just displaying selected-window.
    ;; Needs to happen on a per-window basis.
    ;; The values should depend on where in the tree they are, not menu-bar/mini-buffer.
    (let [window (window/frame-root-window)
          buffer (window/window-buffer window)
          header-line (buffer/buffer-local-value 'header-line-format buffer)
          mode-line (buffer/buffer-local-value 'mode-line-format buffer)]
      (reset! (.top-line window) (+ menu-bar (if header-line 1 0)))
      (reset! (.left-col window) 0) ;; Can't just be 0 of course.
      ;; "normal" size is a weight between 0 - 1.0
      (reset! (.total-cols window) (long (* @(.normal-cols window) width)))
      (reset! (.total-lines window) (long (* @(.normal-lines window) (+ (dec mini-buffer) menu-bar))))


      (let [line-indexes ((ns-resolve 'deuce.emacs.cmds 'line-indexes)
                          (str (.beg (.own-text buffer))))
            pt @(.pt buffer)
            line ((ns-resolve 'deuce.emacs.cmds 'pos-to-line) line-indexes pt)
            total-lines (- @(.total-lines window) (count (remove nil? #{header-line mode-line})))
            scroll (max (- line total-lines) 0)]
        ;; Window Text
        (let [text (.beg (.own-text buffer))
              lines (s/split text #"\n")
              cols @(.total-cols window)
              top-line @(.top-line window)
              top-line (if header-line (inc top-line) top-line)]

          (when header-line
            (puts 0 (dec top-line) (pad (xdisp/format-mode-line header-line nil window buffer) cols) reverse-video))

          (dotimes [n total-lines]
            (puts 0 (+ top-line n) (pad (nth lines (+ scroll n) " ") cols)))

          ;; Point
          (let [[px py] ((ns-resolve 'deuce.emacs.cmds 'point-coords) line-indexes (dec pt))
                py (inc (- py scroll))
                [px py] [(max px 0) (- py (if menu-bar-mode 0 1))]]
            (if (= window (window/selected-window))
              (sc/move-cursor screen px py)
              (sc/put-string screen px py (str (nth text (dec pt))) reverse-video)))

          ;; Mode Line - using total-lines is wrong here, as the reset! logic above assumes one large window.
          (when mode-line
            (puts 0 total-lines (pad (xdisp/format-mode-line mode-line nil window buffer) cols) reverse-video)))))

    ;; Mini Buffer - Can also have a point. And how to tell it's active?
    (dotimes [n @(.total-lines mini-buffer-window)]
      (puts 0 (+ n mini-buffer)
            (pad (.beg (.own-text (window/window-buffer mini-buffer-window))) width)))

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

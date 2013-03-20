(ns deuce.main
  (:require [clojure.string :as s]
            [lanterna.screen :as sc]
            [lanterna.terminal :as te]
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
      (s/join " " menu-bar))))

;; Renders a single window using Lanterna. Scrolling is not properly taken care of.
;; Hard to bootstrap, requires fiddling when connected to Swank inside Deuce atm.
(defn display-using-lanterna []
  (def colors {:bg :default :fg :default})
  (def reverse-video {:fg :white :bg :black})
  (declare screen)

  (defn puts
    ([x y s] (puts x y s colors))
    ([x y s opts] (sc/put-string screen x y (str s) opts)))

  ;; If the screen gets messed up by other output like a stack trace you need to call this.
  (defn blank
    ([] (apply blank (sc/get-size screen)))
    ([_ height]
       (sc/clear screen)
       (sc/redraw screen)))

  (defn pad [s cols]
    (format (str "%-" cols "s") s))

  (defn render-live-window [window]
    (let [buffer (window/window-buffer window)
          minibuffer? (window/window-minibuffer-p window)
          [header-line mode-line] (when-not minibuffer?
                                    [(buffer/buffer-local-value 'header-line-format buffer)
                                     (buffer/buffer-local-value 'mode-line-format buffer)])
          line-indexes ((ns-resolve 'deuce.emacs.cmds 'line-indexes)
                        (str (.beg (.own-text buffer))))
          pt @(.pt buffer)
          line ((ns-resolve 'deuce.emacs.cmds 'pos-to-line) line-indexes pt)
          total-lines (- @(.total-lines window) (or (count (remove nil? [header-line mode-line])) 0))
          scroll (max (- line total-lines) 0)]
      (let [text (.beg (.own-text buffer))
            lines (s/split text #"\n")
            cols @(.total-cols window)
            top-line @(.top-line window)
            top-line (if header-line (inc top-line) top-line)]

        (when header-line
          (puts 0 (dec top-line) (pad (xdisp/format-mode-line header-line nil window buffer) cols) reverse-video))

        (dotimes [n total-lines]
          (puts 0 (+ top-line n) (pad (nth lines (+ scroll n) " ") cols)))

        (let [[px py] ((ns-resolve 'deuce.emacs.cmds 'point-coords) line-indexes (dec pt))
              py (+ top-line (- py scroll))]
          (if (= window (window/selected-window))
            (sc/move-cursor screen px py)
            (sc/put-string screen px py (str (nth text (dec pt) "")) reverse-video)))

        (when mode-line
          (puts 0 (+ top-line total-lines) (pad (xdisp/format-mode-line mode-line nil window buffer) cols) {:bg :white})))))

  (defn render-window [window x y width height]
    ;; We should walk the tree, splitting windows as we go.
    ;; top or left children in turn have next siblings all sharing this area.
    ;; A live window is a normal window with buffer.
    (reset! (.top-line window) y)
    (reset! (.left-col window) x)
    ;; "normal" size is a weight between 0 - 1.0, should hopfully add up.
    (reset! (.total-cols window) (long (* @(.normal-cols window) width)))
    (reset! (.total-lines window) (long (* @(.normal-lines window) height)))

    (condp some [window]
      window/window-live-p (render-live-window window)
      window/window-top-child (throw (UnsupportedOperationException.))
      window/window-left-child (throw (UnsupportedOperationException.))))

  (def screen (terminal/frame-terminal))
  (let [[width height] (te/get-size (.getTerminal screen))
        mini-buffer-window (window/minibuffer-window)
        mini-buffer (- height (window/window-total-height mini-buffer-window))
        menu-bar-mode (data/symbol-value 'menu-bar-mode)
        menu-bar (if menu-bar-mode 1 0)]

    (when menu-bar-mode
      (puts 0 0 (pad (render-menu-bar) width) reverse-video))

    (render-window (window/frame-root-window) 0 menu-bar
                   width (- mini-buffer menu-bar))
    (render-window (window/minibuffer-window) 0 mini-buffer
                   width (window/window-total-height mini-buffer-window))

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

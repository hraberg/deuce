(ns deuce.test.terminal
  (require [lanterna.screen :as s])
  (import [sun.misc Signal SignalHandler]))

;; This is a clojure-lanterna[1] UI spike.

;;   lein trampoline run -m deuce.test.terminal

;; Press C-c C-x to exit. M-x for minibuffer
;; You can type text (insert mode) and move the cursor around. The line number changes on the mode line.

;; This is *NOT* Emacs, Deuce or anything close like it, even though it looks like it at first sight.
;; It is meant to ensure that Lanterna can handle the UI updates Emacs requires.

;; It flickers a bit, but works better than expected. Also, :white isn't white (but :default can be).
;; In theory you can also specify :swing as an argument, but it doesn't work for some reason.

;; I plan to keep extending this spike to scrolling, complex keyboard handling and other risk areas.

;; [1] https://github.com/sjl/clojure-lanterna/ "A Clojurey wrapper around the Lanterna terminal output library."


(def colors {:fg :black :bg :white})
(def reverse-video {:fg (:bg colors) :bg (:fg colors)})
(declare screen)
(def size (atom [80 20]))

(defn puts
  ([x y s] (puts x y s colors))
  ([x y s opts] (s/put-string screen x y (str s) opts)))

(defn line
  ([y] (line y colors))
  ([y opts]
     (let [[width _] @size]
       (puts 0 y (apply str (repeat width " ")) opts))))

(defn blank [_ height]
  (s/clear screen)
  (s/redraw screen)
  (doseq [y (range 0 height)]
    (line y)))

(defn mode-line [line]
  (let [[width height] @size
        text"-UUU:----F1  *scratch*      All L%-6d     (Lisp Interaction)%s"
        padding (apply str (repeat (- width (count text)) "-"))]
    (puts 0 (- height 2) (format text line padding) reverse-video)))

(def current-prompt (atom nil))

(defn mini-buffer [line]
  (let [[width height] @size]
    (puts 0 (- height 1) line)))

(defn clear-mini-buffer []
  (reset! current-prompt nil)
  (let [[width height] @size]
    (line (- height 1))))

(defn move-cursor [x y]
  (s/move-cursor screen x y)
  (mode-line y))

(defn cursor-position []
  [(.getColumn (.getCursorPosition screen))
   (.getRow (.getCursorPosition screen))])

(defn prompt [line fn]
  (mini-buffer line)
  (reset! current-prompt fn))

(defn scratch [_ height]
  (line 0 reverse-video)
  (puts 0 0 "File Edit Options Buffers Tools Lisp-Interaction Help" reverse-video)

  (puts 0 1  ";; This buffer is for notes you don't want to save, and for Lisp evaluation.")
  (puts 0 2  ";; If you want to create a file, visit that file with C-x C-f,")
  (puts 0 3  ";; then enter the text in that file's own buffer.")
  (move-cursor 0 5))

(defn disclaimer []
  (puts 0 0 "Deuce clojure-lanterna Screen Test")
  (puts 0 1 "This is *NOT* Deuce or Emacs")
  (puts 0 2 "Press any key to continue...")
  (s/redraw screen)
  (s/get-key-blocking screen))

(defn resize-screen [width height]
  (reset! size [width height])
  (blank width height)
  (disclaimer)

  (scratch width height)
  (s/redraw screen))

(defn refresh [& _]
  (s/redraw screen)
  (Thread/yield))

(def running (atom true))

(defn exit []
  (reset! running false)
  (s/stop screen)
  :exit)

(defn prompt-exit []
  (prompt "Active processes exists; kill them and exit anyway? (y or n)" exit))

(defn handle-prompt [k f]
  (case (str k)
    "y" (do (clear-mini-buffer)
            (f))
    "n" (clear-mini-buffer)
    nil))

(def mini-buffer-active (atom nil))

(defn activate-mini-buffer [cursor-position]
  (reset! mini-buffer-active cursor-position)
  (mini-buffer "M-x")
  (let [[_ height] @size]
    (move-cursor 4 (- height 1))))

(defn deactivate-mini-buffer []
  (clear-mini-buffer)
  (apply move-cursor @mini-buffer-active)
  (reset! mini-buffer-active nil))

(defn handle-mini-buffer [k [cx cy]]
  (let [[width height] @size]
    (case k
      :down nil
      :up nil
      :right (when (< cx width)
               (move-cursor (inc cx) cy))
      :left (when (> cx 0)
              (move-cursor (dec cx) cy))
      :enter (deactivate-mini-buffer)
      :backspace (when (> cx 1)
                   (puts (dec  cx) cy " ")
                   (move-cursor (dec cx) cy))
      :escape (do (deactivate-mini-buffer)
                  (mini-buffer "Quit"))
      (do
        (puts cx cy k)
        (move-cursor (inc cx) cy)))))

(def key-state (atom nil))

(defn to-ctrl-char [c]
  (char (- (int c) 96)))

(defn ctrl-char? [c]
  (Character/isISOControl c))

(defn from-ctrl-char [c]
  (when (<= (int c) (int \))
    (char (+ (int c) 96))))

(defn to-readable-char [c]
  (condp some [c]
    keyword? (format "<%s>" (name c))
    ctrl-char? (str "C-" (from-ctrl-char c))
    c))

(defn start-chord [l k]
  (reset! key-state k)
  (mini-buffer l))

(defn end-chord []
  (start-chord nil nil))

(defn handle-chord [prefix k [cx cy]]
  (end-chord)
  (case prefix
    \ (case k
          \ (prompt-exit)
          (mini-buffer (format "C-x %s is undefined" (to-readable-char k))))
    \ (mini-buffer (format "C-x %s is undefined" (to-readable-char k)))
    :escape (case k
              \x (activate-mini-buffer [cx cy])
              nil)
    nil))

(defn key-press [k]
  (let [[width height] @size
        [cx cy] (cursor-position)]
    (cond
     @current-prompt (handle-prompt k @current-prompt)
     @key-state (handle-chord @key-state k [cx cy])
     @mini-buffer-active (handle-mini-buffer k [cx cy])
     :else (do
             (clear-mini-buffer)
             (case k
               \ (start-chord "C-x-" \)
               \ (start-chord "C-c-" \)
               :down (when (< cy (- height 3))
                       (move-cursor cx (inc cy)))
               :up (when (> cy 1)
                     (move-cursor cx (dec cy)))
               :right (when (< cx width)
                        (move-cursor (inc cx) cy))
               :left (when (> cx 0)
                       (move-cursor (dec cx) cy))
               :enter (when (< cy (- height 3))
                        (move-cursor 0 (inc cy)))
               :backspace (when (> cx 1)
                            (puts (dec  cx) cy " ")
                            (move-cursor (dec cx) cy))
               :escape (start-chord "" :escape)
               (do
                 (puts cx cy k)
                 (move-cursor (inc cx) cy)))))))

(defn handle-ctrl-c []
  (-> (Runtime/getRuntime) (.addShutdownHook (Thread. #(while @running (Thread/sleep 100)))))
  (Signal/handle (Signal. "INT")
                 (proxy [SignalHandler] []
                   (handle [s]
                     (key-press \)
                     (refresh)))))

(defn -main [& [screen-type]]
  (def screen (s/get-screen (read-string (or screen-type ":text"))))
  (s/add-resize-listener screen resize-screen)
  (handle-ctrl-c)
  (s/in-screen screen
               (->> (repeatedly #(s/get-key screen))
                    (remove nil?)
                    (map key-press)
                    (take-while (complement #{:exit}))
                    (map refresh)
                    dorun))
  (System/exit 0))

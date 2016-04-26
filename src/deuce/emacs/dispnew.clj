(ns deuce.emacs.dispnew
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c]
            [clojure.string :as s]
            [deuce.emacs.buffer :as buffer]
            [deuce.emacs.data :as data]
            [deuce.emacs.editfns :as editfns]
            [deuce.emacs.fns :as fns]
            [deuce.emacs.frame :as frame]
            [deuce.emacs.keymap :as keymap]
            [deuce.emacs.terminal :as terminal]
            [deuce.emacs.window :as window]
            [deuce.emacs.xdisp :as xdisp]
            [deuce.emacs-lisp.parser :as parser])
  (:import [java.util HashSet]
           [com.googlecode.lanterna TerminalPosition SGR]
           [com.googlecode.lanterna TextColor$ANSI]
           [com.googlecode.lanterna.graphics TextGraphics]
           [com.googlecode.lanterna.screen Screen]
           [deuce.emacs.data Buffer Window])
  (:refer-clojure :exclude []))

(defvar no-redraw-on-reenter nil
  "Non-nil means no need to redraw entire frame after suspending.
  A non-nil value is useful if the terminal can automatically preserve
  Emacs's frame display when you reenter Emacs.
  It is up to you to set this variable if your terminal can do that.

  You can customize this variable.")

(defvar cursor-in-echo-area nil
  "Non-nil means put cursor in minibuffer, at end of any message there.")

(defvar standard-display-table nil
  "Display table to use for buffers that specify none.
  See `buffer-display-table' for more information.")

(defvar visible-bell nil
  "Non-nil means try to flash the frame to represent a bell.

  See also `ring-bell-function'.

  You can customize this variable.")

(defvar redisplay-dont-pause nil
  "Non-nil means display update isn't paused when input is detected.")

(defvar redisplay-preemption-period nil
  "Period in seconds between checking for input during redisplay.
  This has an effect only if `redisplay-dont-pause' is nil; in that
  case, arriving input preempts redisplay until the input is processed.
  If the value is nil, redisplay is never preempted.")

(defvar window-system-version nil
  "The version number of the window system in use.
  For X windows, this is 11.")

(defvar initial-window-system nil
  "Name of the window system that Emacs uses for the first frame.
  The value is a symbol:
   nil for a termcap frame (a character-only terminal),
   'x' for an Emacs frame that is really an X window,
   'w32' for an Emacs frame that is a window on MS-Windows display,
   'ns' for an Emacs frame on a GNUstep or Macintosh Cocoa display,
   'pc' for a direct-write MS-DOS frame.

  Use of this variable as a boolean is deprecated.  Instead,
  use `display-graphic-p' or any of the other `display-*-p'
  predicates which report frame's specific UI-related capabilities.")

(defvar baud-rate nil
  "The output baud rate of the terminal.
  On most systems, changing this value will affect the amount of padding
  and the other strategic decisions made during redisplay.

  You can customize this variable.")

(defvar glyph-table nil
  "Table defining how to output a glyph code to the frame.
  If not nil, this is a vector indexed by glyph code to define the glyph.
  Each element can be:
   integer: a glyph code which this glyph is an alias for.
   string: output this glyph using that string (not impl. in X windows).
   nil: this glyph mod 524288 is the code of a character to output,
      and this glyph / 524288 is the face number (see `face-id') to use
      while outputting it.")

(defvar inverse-video nil
  "Non-nil means invert the entire frame display.
  This means everything is in inverse video which otherwise would not be.

  You can customize this variable.")

(defun open-termscript (file)
  "Start writing all terminal output to FILE as well as the terminal.
  FILE = nil means just close any termscript file currently open."
  (interactive "FOpen termscript file: "))

(defun ding (&optional arg)
  "Beep, or flash the screen.
  Also, unless an argument is given,
  terminate any keyboard macro currently executing."
  )

(defun internal-show-cursor (window show)
  "Set the cursor-visibility flag of WINDOW to SHOW.
  WINDOW nil means use the selected window.  SHOW non-nil means
  show a cursor in WINDOW in the next redisplay.  SHOW nil means
  don't show a cursor."
  )

;; The way this does this is probably utterly wrong, written by data inspection, not reading Emacs source.
;; But produces the expected result:
(defn ^:private render-menu-bar []
  (when (data/symbol-value 'menu-bar-mode)
    (let [map-for-mode #(let [map (symbol (str % "-map"))]
                          (when (data/boundp map)
                            (data/symbol-value map)))
          ;; The consp check here is suspicious.
          ;; There's a "menu-bar" string in there which probably shouldn't be.
          menus-for-map #(map keymap/keymap-prompt (filter data/consp (fns/nthcdr 2 (fns/assq 'menu-bar %))))
          menu-bar-by-name #(data/symbol-value (symbol (str "menu-bar-" %)))
          final-items (map menu-bar-by-name (data/symbol-value 'menu-bar-final-items))
          final-menus (map keymap/keymap-prompt final-items)
          ;; Hack to create the same display order as Emacs.
          menu-bar (concat (remove (some-fn nil? symbol?) ;; This is to get rid of tmm-menu-bar-mouse
                                   (remove (set final-menus) ;; Remove Help that goes on the end.
                                           (mapcat menus-for-map [(keymap/current-global-map)
                                                                  (keymap/current-local-map)])))
                           final-menus)]
      (s/join " " menu-bar))))

;; Renders a single window using Lanterna. Scrolling is not properly taken care of.
;; Hard to bootstrap, requires fiddling when connected to nREPL inside Deuce atm.
;; Consider moving all this into deuce.emacs.dispnew
(def ^:private reverse-video {:styles #{SGR/REVERSE}})
(def ^:private region-colors {:fg TextColor$ANSI/DEFAULT :bg TextColor$ANSI/YELLOW})

(defn ^:private puts
  ([text-graphics x y s] (puts text-graphics x y s {}))
  ([^TextGraphics text-graphics x y s
    {:keys [styles fg bg] :or {styles #{} fg TextColor$ANSI/DEFAULT bg TextColor$ANSI/DEFAULT}}]
   (.setForegroundColor text-graphics fg)
   (.setBackgroundColor text-graphics bg)
   (.putString text-graphics x y (str s) (HashSet. styles))))

(defn ^:private pad [s cols]
  (format (str "%-" cols "s") s))

(defn ^:private render-live-window [^Screen screen ^TextGraphics text-graphics ^Window window]
  (let [^Buffer buffer (window/window-buffer window)
        minibuffer? (window/window-minibuffer-p window)
        [header-line mode-line] (when-not minibuffer?
                                  [(buffer/buffer-local-value 'header-line-format buffer)
                                   (buffer/buffer-local-value 'mode-line-format buffer)])
        text (binding [buffer/*current-buffer* buffer]
               (editfns/buffer-string))
        line-indexes ((ns-resolve 'deuce.emacs.cmds 'line-indexes) text)
        pos-to-line (partial (ns-resolve 'deuce.emacs.cmds 'pos-to-line) line-indexes)
        point-coords (partial (ns-resolve 'deuce.emacs.cmds 'point-coords) line-indexes)
        pt (- @(.pt buffer) (or @(.begv buffer) 0))
        line (pos-to-line pt)
        total-lines (- @(.total-lines window) (or (count (remove nil? [header-line mode-line])) 0))
        scroll (max (inc (- line total-lines)) 0)
        mark-active? (buffer/buffer-local-value 'mark-active buffer)
        selected-window? (= window (window/selected-window))
        puts (partial puts text-graphics)]
    (let [lines (s/split text #"\n")
          cols @(.total-cols window)
          top-line @(.top-line window)
          top-line (if header-line (inc top-line) top-line)
          screen-coords (fn [[x y]] [x  (+ top-line (- y scroll))])] ;; Not dealing with horizontal scroll.

      (when header-line
        (puts 0 (dec top-line) (pad (xdisp/format-mode-line header-line nil window buffer) cols) reverse-video))

      (let [[[rbx rby] [rex rey]]
            (if (and mark-active? selected-window?)
              [(screen-coords (point-coords (dec (editfns/region-beginning))))
               (screen-coords (point-coords (dec (editfns/region-end))))]
              [[-1 -1] [-1 -1]])]

        (dotimes [n total-lines]
          (let [screen-line (+ top-line n)
                text (pad (nth lines (+ scroll n) " ") cols)]
            (cond
             (= screen-line rby rey) (do
                                       (puts 0 screen-line (subs text 0 rbx))
                                       (puts rbx screen-line (subs text rbx rex) region-colors)
                                       (puts rex screen-line (subs text rex)))

             (= screen-line rby) (do
                                   (puts 0 screen-line (subs text 0 rbx))
                                   (puts rbx screen-line (subs text rbx) region-colors))

             (= screen-line rey) (do
                                   (puts 0 screen-line (subs text 0 rex) region-colors)
                                   (puts rex screen-line (subs text rex)))

             (< rby screen-line rey) (puts 0 screen-line text region-colors)

             :else (puts 0 screen-line text)))))

      (when selected-window?
        (let [[px py] (screen-coords (point-coords (dec pt)))]
          (.setCursorPosition screen (TerminalPosition. px py))))

      (when mode-line
        (puts 0 (+ top-line total-lines)
              (pad (xdisp/format-mode-line mode-line nil window buffer) cols)
              {:bg TextColor$ANSI/WHITE})))))

(defn ^:private render-window [^Screen screen ^TextGraphics text-graphics ^Window window x y width height]
  ;; We should walk the tree, splitting windows as we go.
  ;; top or left children in turn have next siblings all sharing this area.
  ;; A live window is a normal window with buffer.
  (reset! (.top-line window) y)
  (reset! (.left-col window) x)
  ;; "normal" size is a weight between 0 - 1.0, should hopfully add up.
  (reset! (.total-cols window) (long (* @(.normal-cols window) width)))
  (reset! (.total-lines window) (long (* @(.normal-lines window) height)))

  (condp some [window]
    window/window-live-p (render-live-window screen text-graphics window)
    window/window-top-child (throw (UnsupportedOperationException.))
    window/window-left-child (throw (UnsupportedOperationException.))))

(defun redraw-frame (frame)
  "Clear frame FRAME and output again what is supposed to appear on it."
  (let [screen ^Screen (terminal/frame-terminal frame)
        text-graphics (.newTextGraphics screen)
        {:keys [rows columns]} (bean (.getTerminalSize screen))
        mini-buffer-window (window/minibuffer-window)
        mini-buffer (- rows (window/window-total-height mini-buffer-window))
        menu-bar-mode (data/symbol-value 'menu-bar-mode)
        menu-bar (if menu-bar-mode 1 0)]

    (when menu-bar-mode
      (puts text-graphics 0 0 (pad (render-menu-bar) columns) reverse-video))

    (render-window screen text-graphics
                   (window/frame-root-window) 0 menu-bar
                   columns (- mini-buffer menu-bar))
    (render-window screen text-graphics
                   (window/minibuffer-window) 0 mini-buffer
                   columns (window/window-total-height mini-buffer-window))

    (.refresh screen)))

(defun frame-or-buffer-changed-p (&optional variable)
  "Return non-nil if the frame and buffer state appears to have changed.
  VARIABLE is a variable name whose value is either nil or a state vector
  that will be updated to contain all frames and buffers,
  aside from buffers whose names start with space,
  along with the buffers' read-only and modified flags.  This allows a fast
  check to see whether buffer menus might need to be recomputed.
  If this function returns non-nil, it updates the internal vector to reflect
  the current state.

  If VARIABLE is nil, an internal variable is used.  Users should not
  pass nil for VARIABLE."
  )

(defun redisplay (&optional force)
  "Perform redisplay.
  Optional arg FORCE, if non-nil, prevents redisplay from being
  preempted by arriving input, even if `redisplay-dont-pause' is nil.
  If `redisplay-dont-pause' is non-nil (the default), redisplay is never
  preempted by arriving input, so FORCE does nothing.

  Return t if redisplay was performed, nil if redisplay was preempted
  immediately by pending input."
  (redraw-frame (frame/selected-frame)))

(defun internal-show-cursor-p (&optional window)
  "Value is non-nil if next redisplay will display a cursor in WINDOW.
  WINDOW nil or omitted means report on the selected window."
  )

(defun last-nonminibuffer-frame ()
  "Value is last nonminibuffer frame."
  (frame/selected-frame))

(defun send-string-to-terminal (string &optional terminal)
  "Send STRING to the terminal without alteration.
  Control characters in STRING will have terminal-dependent effects.

  Optional parameter TERMINAL specifies the tty terminal device to use.
  It may be a terminal object, a frame, or nil for the terminal used by
  the currently selected frame.  In batch mode, STRING is sent to stdout
  when TERMINAL is nil."
  (.print System/out (parser/resolve-control-chars string))
  (.flush System/out))

(defun redraw-display ()
  "Clear and redisplay all visible frames."
  (interactive)
  (when-let [screen ^Screen (terminal/frame-terminal)]
    (.clear screen)
    (.clearScreen (.getTerminal screen))
    (.refresh screen)
    (redisplay)))

(defun sleep-for (seconds &optional milliseconds)
  "Pause, without updating display, for SECONDS seconds.
  SECONDS may be a floating-point value, meaning that you can wait for a
  fraction of a second.  Optional second arg MILLISECONDS specifies an
  additional wait period, in milliseconds; this may be useful if your
  Emacs was built without floating point support.
  (Not all operating systems support waiting for a fraction of a second.)"
  (Thread/sleep (+ (* 1000 seconds) (or milliseconds) 0)))

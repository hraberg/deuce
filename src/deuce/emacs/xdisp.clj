(ns deuce.emacs.xdisp
  (:use [deuce.emacs-lisp :only (defun defvar) :as el])
  (:require [clojure.core :as c]
            [clojure.string :as s]
            [deuce.emacs.buffer :as buffer]
            [deuce.emacs.data :as data]
            [deuce.emacs.editfns :as editfns]
            [deuce.emacs.eval :as eval]
            [deuce.emacs.fileio :as fileio]
            [deuce.emacs.frame :as frame]
            [deuce.emacs.keyboard :as keyboard]
            [deuce.emacs.window :as window])
  (:refer-clojure :exclude []))

(defvar scroll-step nil
  "*The number of lines to try scrolling a window by when point moves out.
  If that fails to bring point back on frame, point is centered instead.
  If this is zero, point is always centered after it moves off frame.
  If you want scrolling to always be a line at a time, you should set
  `scroll-conservatively' to a large value rather than set this to 1.

  You can customize this variable.")

(defvar display-hourglass nil
  "Non-nil means show an hourglass pointer, when Emacs is busy.
  This feature only works when on a window system that can change
  cursor shapes.

  You can customize this variable.")

(defvar inhibit-redisplay nil
  "Non-nil means don't actually do any redisplay.
  This is used for internal purposes.")

(defvar inhibit-eval-during-redisplay nil
  "Non-nil means don't eval Lisp during redisplay.")

(defvar wrap-prefix nil
  "Prefix prepended to all continuation lines at display time.
  The value may be a string, an image, or a stretch-glyph; it is
  interpreted in the same way as the value of a `display' text property.

  This variable is overridden by any `wrap-prefix' text or overlay
  property.

  To add a prefix to non-continuation lines, use `line-prefix'.")

(defvar show-trailing-whitespace nil
  "*Non-nil means highlight trailing whitespace.
  The face used for trailing whitespace is `trailing-whitespace'.

  You can customize this variable.")

(defvar void-text-area-pointer nil
  "*The pointer shape to show in void text areas.
  A value of nil means to show the text pointer.  Other options are `arrow',
  `text', `hand', `vdrag', `hdrag', `modeline', and `hourglass'.")

(defvar blink-cursor-alist nil
  "Alist specifying how to blink the cursor off.
  Each element has the form (ON-STATE . OFF-STATE).  Whenever the
  `cursor-type' frame-parameter or variable equals ON-STATE,
  comparing using `equal', Emacs uses OFF-STATE to specify
  how to blink it off.  ON-STATE and OFF-STATE are values for
  the `cursor-type' frame parameter.

  If a frame's ON-STATE has no entry in this list,
  the frame's other specifications determine how to blink the cursor off.

  You can customize this variable.")

(defvar auto-hscroll-mode nil
  "Allow or disallow automatic horizontal scrolling of windows.
  If non-nil, windows are automatically scrolled horizontally to make
  point visible.

  You can customize this variable.")

(defvar inhibit-free-realized-faces nil
  "Non-nil means don't free realized faces.  Internal use only.")

(defvar line-number-display-limit nil
  "*Maximum buffer size for which line number should be displayed.
  If the buffer is bigger than this, the line number does not appear
  in the mode line.  A value of nil means no limit.

  You can customize this variable.")

(defvar menu-bar-update-hook nil
  "Normal hook run to update the menu bar definitions.
  Redisplay runs this hook before it redisplays the menu bar.
  This is used to update submenus such as Buffers,
  whose contents depend on various data.")

(defvar window-text-change-functions nil
  "Functions to call in redisplay when text in the window might change.")

(defvar scroll-margin nil
  "*Number of lines of margin at the top and bottom of a window.
  Recenter the window whenever point gets within this many lines
  of the top or bottom of the window.

  You can customize this variable.")

(defvar message-log-max nil
  "Maximum number of lines to keep in the message log buffer.
  If nil, disable message logging.  If t, log messages but don't truncate
  the buffer when it becomes large.

  You can customize this variable.")

(defvar tool-bar-border nil
  "*Border below tool-bar in pixels.
  If an integer, use it as the height of the border.
  If it is one of `internal-border-width' or `border-width', use the
  value of the corresponding frame parameter.
  Otherwise, no border is added below the tool-bar.")

(defvar hscroll-margin nil
  "*How many columns away from the window edge point is allowed to get
  before automatic hscrolling will horizontally scroll the window.

  You can customize this variable.")

(defvar scroll-conservatively nil
  "*Scroll up to this many lines, to bring point back on screen.
  If point moves off-screen, redisplay will scroll by up to
  `scroll-conservatively' lines in order to bring point just barely
  onto the screen again.  If that cannot be done, then redisplay
  recenters point as usual.

  If the value is greater than 100, redisplay will never recenter point,
  but will always scroll just enough text to bring point into view, even
  if you move far away.

  A value of zero means always recenter point if it moves off screen.

  You can customize this variable.")

(defvar unibyte-display-via-language-environment nil
  "*Non-nil means display unibyte text according to language environment.
  Specifically, this means that raw bytes in the range 160-255 decimal
  are displayed by converting them to the equivalent multibyte characters
  according to the current language environment.  As a result, they are
  displayed according to the current fontset.

  Note that this variable affects only how these bytes are displayed,
  but does not change the fact they are interpreted as raw bytes.

  You can customize this variable.")

(defvar tool-bar-button-relief nil
  "*Relief thickness of tool-bar buttons.")

(defvar tool-bar-button-margin nil
  "*Margin around tool-bar buttons in pixels.
  If an integer, use that for both horizontal and vertical margins.
  Otherwise, value should be a pair of integers `(HORZ . VERT)' with
  HORZ specifying the horizontal margin, and VERT specifying the
  vertical margin.")

(defvar message-truncate-lines nil
  "If non-nil, messages are truncated instead of resizing the echo area.
  Bind this around calls to `message' to let it take effect.")

(defvar tool-bar-max-label-size nil
  "*Maximum number of characters a label can have to be shown.
  The tool bar style must also show labels for this to have any effect, see
  `tool-bar-style'.

  You can customize this variable.")

(defvar line-prefix nil
  "Prefix prepended to all non-continuation lines at display time.
  The value may be a string, an image, or a stretch-glyph; it is
  interpreted in the same way as the value of a `display' text property.

  This variable is overridden by any `line-prefix' text or overlay
  property.

  To add a prefix to continuation lines, use `wrap-prefix'.")

(defvar redisplay-end-trigger-functions nil
  "Functions called when redisplay of a window reaches the end trigger.
  Each function is called with two arguments, the window and the end trigger value.
  See `set-window-redisplay-end-trigger'.")

(defvar max-mini-window-height nil
  "*Maximum height for resizing mini-windows (the minibuffer and the echo area).
  If a float, it specifies a fraction of the mini-window frame's height.
  If an integer, it specifies a number of lines.

  You can customize this variable.")

(defvar overlay-arrow-position nil
  "Marker for where to display an arrow on top of the buffer text.
  This must be the beginning of a line in order to work.
  See also `overlay-arrow-string'.")

(defvar icon-title-format nil
  "Template for displaying the title bar of an iconified frame.
  (Assuming the window manager supports this feature.)
  This variable has the same structure as `mode-line-format' (which see),
  and is used only on frames for which no explicit name has been set
  (see `modify-frame-parameters').")

(defvar overlay-arrow-variable-list nil
  "List of variables (symbols) which hold markers for overlay arrows.
  The symbols on this list are examined during redisplay to determine
  where to display overlay arrows.")

(defvar truncate-partial-width-windows nil
  "Non-nil means truncate lines in windows narrower than the frame.
  For an integer value, truncate lines in each window narrower than the
  full frame width, provided the window width is less than that integer;
  otherwise, respect the value of `truncate-lines'.

  For any other non-nil value, truncate lines in all windows that do
  not span the full frame width.

  A value of nil means to respect the value of `truncate-lines'.

  If `word-wrap' is enabled, you might want to reduce this.

  You can customize this variable.")

(defvar make-cursor-line-fully-visible nil
  "*Non-nil means to scroll (recenter) cursor line if it is not fully visible.")

(defvar hourglass-delay nil
  "*Seconds to wait before displaying an hourglass pointer when Emacs is busy.

  You can customize this variable.")

(defvar inhibit-menubar-update nil
  "Non-nil means don't update menu bars.  Internal use only.")

(defvar display-pixels-per-inch nil
  "Pixels per inch value for non-window system displays.
  Value is a number or a cons (WIDTH-DPI . HEIGHT-DPI).")

(defvar auto-raise-tool-bar-buttons nil
  "*Non-nil means raise tool-bar buttons when the mouse moves over them.")

(defvar nobreak-char-display nil
  "Control highlighting of non-ASCII space and hyphen chars.
  If the value is t, Emacs highlights non-ASCII chars which have the
  same appearance as an ASCII space or hyphen, using the `nobreak-space'
  or `escape-glyph' face respectively.

  U+00A0 (no-break space), U+00AD (soft hyphen), U+2010 (hyphen), and
  U+2011 (non-breaking hyphen) are affected.

  Any other non-nil value means to display these characters as a escape
  glyph followed by an ordinary space or hyphen.

  A value of nil means no special handling of these characters.")

(defvar auto-resize-tool-bars nil
  "*Non-nil means automatically resize tool-bars.
  This dynamically changes the tool-bar's height to the minimum height
  that is needed to make all tool-bar items visible.
  If value is `grow-only', the tool-bar's height is only increased
  automatically; to decrease the tool-bar height, use M-x recenter.")

(defvar fontification-functions nil
  "List of functions to call to fontify regions of text.
  Each function is called with one argument POS.  Functions must
  fontify a region starting at POS in the current buffer, and give
  fontified regions the property `fontified'.")

(defvar hscroll-step nil
  "*How many columns to scroll the window when point gets too close to the edge.
  When point is less than `hscroll-margin' columns from the window
  edge, automatic hscrolling will scroll the window by the amount of columns
  determined by this variable.  If its value is a positive integer, scroll that
  many columns.  If it's a positive floating-point number, it specifies the
  fraction of the window's width to scroll.  If it's nil or zero, point will be
  centered horizontally after the scroll.  Any other value, including negative
  numbers, are treated as if the value were zero.

  Automatic hscrolling always moves point outside the scroll margin, so if
  point was more than scroll step columns inside the margin, the window will
  scroll more than the value given by the scroll step.

  Note that the lower bound for automatic hscrolling specified by `scroll-left'
  and `scroll-right' overrides this variable's effect.

  You can customize this variable.")

(defvar overlay-arrow-string nil
  "String to display as an arrow in non-window frames.
  See also `overlay-arrow-position'.")

(defvar window-scroll-functions nil
  "List of functions to call before redisplaying a window with scrolling.
  Each function is called with two arguments, the window and its new
  display-start position.  Note that these functions are also called by
  `set-window-buffer'.  Also note that the value of `window-end' is not
  valid when these functions are called.

  Warning: Do not use this feature to alter the way the window
  is scrolled.  It is not designed for that, and such use probably won't
  work.")

(defvar frame-title-format nil
  "Template for displaying the title bar of visible frames.
  (Assuming the window manager supports this feature.)

  This variable has the same structure as `mode-line-format', except that
  the %c and %l constructs are ignored.  It is used only on frames for
  which no explicit name has been set (see `modify-frame-parameters').")

(defvar mouse-autoselect-window nil
  "*Non-nil means autoselect window with mouse pointer.
  If nil, do not autoselect windows.
  A positive number means delay autoselection by that many seconds: a
  window is autoselected only after the mouse has remained in that
  window for the duration of the delay.
  A negative number has a similar effect, but causes windows to be
  autoselected only after the mouse has stopped moving.  (Because of
  the way Emacs compares mouse events, you will occasionally wait twice
  that time before the window gets selected.)
  Any other value means to autoselect window instantaneously when the
  mouse pointer enters it.

  Autoselection selects the minibuffer only if it is active, and never
  unselects the minibuffer if it is active.

  When customizing this variable make sure that the actual value of
  `focus-follows-mouse' matches the behavior of your window manager.

  You can customize this variable.")

(defvar window-size-change-functions nil
  "Functions called before redisplay, if window sizes have changed.
  The value should be a list of functions that take one argument.
  Just before redisplay, for each frame, if any of its windows have changed
  size since the last redisplay, or have been split or deleted,
  all the functions in the list are called, with the frame as argument.")

(defvar menu-updating-frame nil
  "Frame for which we are updating a menu.
  The enable predicate for a menu binding should check this variable.")

(defvar multiple-frames nil
  "Non-nil if more than one frame is visible on this display.
  Minibuffer-only frames don't count, but iconified frames do.
  This variable is not guaranteed to be accurate except while processing
  `frame-title-format' and `icon-title-format'.")

(defvar mode-line-inverse-video nil
  "When nil, display the mode-line/header-line/menu-bar in the default face.
  Any other value means to use the appropriate face, `mode-line',
  `header-line', or `menu' respectively.

  You can customize this variable.")

(defvar tool-bar-style nil
  "Tool bar style to use.
  It can be one of
   image            - show images only
   text             - show text only
   both             - show both, text below image
   both-horiz       - show text to the right of the image
   text-image-horiz - show text to the left of the image
   any other        - use system default or image if no system default.

  You can customize this variable.")

(defvar resize-mini-windows nil
  "How to resize mini-windows (the minibuffer and the echo area).
  A value of nil means don't automatically resize mini-windows.
  A value of t means resize them to fit the text displayed in them.
  A value of `grow-only', the default, means let mini-windows grow only;
  they return to their normal size when the minibuffer is closed, or the
  echo area becomes empty.")

(defvar global-mode-string nil
  "String (or mode line construct) included (normally) in `mode-line-format'.")

(defvar line-number-display-limit-width nil
  "*Maximum line width (in characters) for line number display.
  If the average length of the lines near point is bigger than this, then the
  line number may be omitted from the mode line.

  You can customize this variable.")

(defvar underline-minimum-offset nil
  "Minimum distance between baseline and underline.
  This can improve legibility of underlined text at small font sizes,
  particularly when using variable `x-use-underline-position-properties'
  with fonts that specify an UNDERLINE_POSITION relatively close to the
  baseline.  The default value is 1.

  You can customize this variable.")

(defvar glyphless-char-display nil
  "Char-table defining glyphless characters.
  Each element, if non-nil, should be one of the following:
    an ASCII acronym string: display this string in a box
    `hex-code':   display the hexadecimal code of a character in a box
    `empty-box':  display as an empty box
    `thin-space': display as 1-pixel width space
    `zero-width': don't display
  An element may also be a cons cell (GRAPHICAL . TEXT), which specifies the
  display method for graphical terminals and text terminals respectively.
  GRAPHICAL and TEXT should each have one of the values listed above.

  The char-table has one extra slot to control the display of a character for
  which no font is found.  This slot only takes effect on graphical terminals.
  Its value should be an ASCII acronym string, `hex-code', `empty-box', or
  `thin-space'.  The default is `empty-box'.")

(defvar overline-margin nil
  "*Space between overline and text, in pixels.
  The default value is 2: the height of the overline (1 pixel) plus 1 pixel
  margin to the character height.

  You can customize this variable.")

(defvar highlight-nonselected-windows nil
  "*Non-nil means highlight region even in nonselected windows.

  You can customize this variable.")

(defun current-bidi-paragraph-direction (&optional buffer)
  "Return paragraph direction at point in BUFFER.
  Value is either `left-to-right' or `right-to-left'.
  If BUFFER is omitted or nil, it defaults to the current buffer.

  Paragraph direction determines how the text in the paragraph is displayed.
  In left-to-right paragraphs, text begins at the left margin of the window
  and the reading direction is generally left to right.  In right-to-left
  paragraphs, text begins at the right margin and is read from right to left.

  See also `bidi-paragraph-direction'."
  )

(defun format-mode-line (format &optional face window buffer)
  "Format a string out of a mode line format specification.
  First arg FORMAT specifies the mode line format (see `mode-line-format'
  for details) to use.

  By default, the format is evaluated for the currently selected window.

  Optional second arg FACE specifies the face property to put on all
  characters for which no face is specified.  The value nil means the
  default face.  The value t means whatever face the window's mode line
  currently uses (either `mode-line' or `mode-line-inactive',
  depending on whether the window is the selected window or not).
  An integer value means the value string has no text
  properties.

  Optional third and fourth args WINDOW and BUFFER specify the window
  and buffer to use as the context for the formatting (defaults
  are the selected window and the WINDOW's buffer)."
  (let [window (el/check-type 'windowp (or window (window/selected-window)))
        buffer (el/check-type 'bufferp (or buffer (window/window-buffer window)))
        window-width (max (window/window-total-width window)
                          (data/symbol-value 'fill-column))
        point @(.pt buffer)
        substring (.substring (.beg (.own-text buffer)) 0 (dec point))
        line-offset (.lastIndexOf substring "\n")
        column (dec (if (= -1 line-offset) point (- point line-offset)))
        line (if (= -1 line-offset) 1
               (loop [idx 0 line 1]
                 (if (= idx line-offset) line
                     (recur (unchecked-inc idx)
                            (if (= \newline (.charAt substring idx)) (unchecked-inc line) line)))))
        modified? (buffer/buffer-modified-p buffer)
        read-only? (buffer/buffer-local-value 'buffer-read-only buffer)
        recursion-depth (keyboard/recursion-depth)
        coding-system-mnemonic "1" ;; "1" is ISO-Latin-1, U" is UTF-8, "-" is ASCII. See interntional/mule-conf
        eol-type-mnemnonic ((el/fun 'coding-system-eol-type-mnemonic) nil)
        % (fn [x] (re-pattern (str "%(-?\\d*)" x)))
        humanize (fn [x]
                   (some identity (reverse
                          (map-indexed (fn [idx suffix]
                                         (let [size (Math/pow 1024 idx)]
                                           (when (> x size)
                                             (str (long (/ x size)) suffix))) )
                                       ["" "k" "M" "G"]))))
        pad (fn padder
              ([value] (partial padder value))
              ([value [_ pad]]
                 (let [s (str value)
                       pad (when (seq pad) (Integer/parseInt pad))]
                   (c/format (str "%" (if (or (not pad) (neg? pad)) ""
                                          (if (and (pos? pad) (number? value)) pad (- pad))) "s")
                             (if (and pad (neg? pad))
                               (subs s 0 (min (count s) (- pad)))
                               s)))))
        formatter (fn formatter [f] ;; Vastly incomplete and wrong.
                    (condp some [f]
                      string? (let [%% (str (gensym "PERCENT"))]
                                (-> ;; Deal with %% last.
                                 (reduce #(s/replace %1 (key %2) (val %2)) (s/replace f "%%" %%)
                                         {(% "e") "" ;; "!MEM FULL! "
                                          (% "n") "" ;; "Narrow"
                                          (% "z") coding-system-mnemonic
                                          (% "Z") (str coding-system-mnemonic eol-type-mnemnonic)
                                          (% "\\[") (if (< 5 recursion-depth)
                                                      "[[[... "
                                                      (apply str (repeat (keyboard/recursion-depth) "[" )))
                                          (% "\\]") (if (< 5 recursion-depth)
                                                      " ...]]]"
                                                      (apply str (repeat (keyboard/recursion-depth) "]" )))
                                          (% "@")  "-" ;; files/file-remote-p
                                          (% "\\+") (cond
                                                     modified? "*"
                                                     read-only? "%"
                                                     :else "-")
                                          (% "\\*") (cond
                                                     read-only? "%"
                                                     modified? "*"
                                                     :else "-")
                                          (% "&") (if modified? "*" "-")
                                          (% "l") (str line)
                                          (% "c") (str column)
                                          (% "i") (pad (editfns/buffer-size buffer)) ;; Should take narrowing in account.
                                          (% "I") (pad (humanize (editfns/buffer-size buffer)))
                                          (% "p") (pad (let [percent (long (* 100 (/ @(.pt buffer)
                                                                                     (inc (editfns/buffer-size buffer)))))]
                                                         (case percent
                                                           0 "Top"
                                                           100 "Bottom" ;; or "All" if top line is visible.
                                                           (str percent "%"))))
                                          ;; (% "P") ;; The reverse of the above
                                          (% "m") (pad (buffer/buffer-local-value 'mode-name buffer))
                                          (% "M") (pad (data/symbol-value 'global-mode-string))
                                          (% "b") (pad (buffer/buffer-name buffer))
                                          (% "f") (pad (buffer/buffer-file-name buffer))
                                          (% "F") (pad (.name (frame/selected-frame)))})
                                 (s/replace %% "%")))
                      symbol? (formatter (data/symbol-value f))
                      seq? (let [fst (first f)]
                             (condp some [fst]
                               integer? (pad (formatter (rest f)) [:ignored (str fst)])
                               #{:eval} (formatter (eval/eval (second f)))
                               #{:propertize} (formatter (second f)) ;; Properties are used for tooltips, fonts etc.
                               symbol? (if (and (data/boundp fst) (data/symbol-value fst))
                                         (formatter (second f))
                                         (formatter (nth f 2 nil)))
                               (apply str (map formatter f))))
                      (str f)))]
    (let [mode-line (formatter format)]
      (s/replace mode-line #"%-$"
                 (apply str (repeat (- window-width (count mode-line)) "-"))))))

(defun invisible-p (pos-or-prop)
  "Non-nil if the property makes the text invisible.
  POS-OR-PROP can be a marker or number, in which case it is taken to be
  a position in the current buffer and the value of the `invisible' property
  is checked; or it can be some other value, which is then presumed to be the
  value of the `invisible' property of the text of interest.
  The non-nil value returned can be t for truly invisible text or something
  else if the text is replaced by an ellipsis."
  )

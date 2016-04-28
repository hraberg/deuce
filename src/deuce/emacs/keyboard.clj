(ns deuce.emacs.keyboard
  (:use [deuce.emacs-lisp :only (defun defvar) :as el])
  (:require [clojure.core :as c]
            [clojure.java.shell :as sh]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.buffer :as buffer]
            [deuce.emacs.callint :as callint]
            [deuce.emacs.casefiddle :as casefiddle]
            [deuce.emacs.data :as data]
            [deuce.emacs.editfns :as editfns]
            [deuce.emacs.eval :as eval]
            [deuce.emacs.frame :as frame]
            [deuce.emacs.keymap :as keymap]
            [deuce.emacs.macros :as macros]
            [deuce.emacs.print :as print]
            [deuce.emacs.term :as term]
            [deuce.emacs.terminal :as terminal]
            [deuce.emacs.window :as window]
            [deuce.emacs-lisp.parser :as parser]
            [taoensso.timbre :as timbre])
  (:import [sun.misc Signal SignalHandler]
           [java.io InputStreamReader]
           [clojure.lang ExceptionInfo])
  (:refer-clojure :exclude []))

(defvar last-command-event nil
  "Last input event that was part of a command.")

(defvar unread-command-char nil
  "If not -1, an object to be read as next command input event.")

(defvar deferred-action-function nil
  "Function to call to handle deferred actions, after each command.
  This function is called with no arguments after each command
  whenever `deferred-action-list' is non-nil.")

(defvar last-input-event nil
  "Last input event.")

(defvar lucid-menu-bar-dirty-flag nil
  "Non-nil means menu bar, specified Lucid style, needs to be recomputed.")

(defvar auto-save-interval nil
  "Number of input events between auto-saves.
  Zero means disable autosaving due to number of characters typed.

  You can customize this variable.")

(defvar track-mouse nil
  "Non-nil means generate motion events for mouse motion.")

(defvar this-command-keys-shift-translated nil
  "Non-nil if the key sequence activating this command was shift-translated.
  Shift-translation occurs when there is no binding for the key sequence
  as entered, but a binding was found by changing an upper-case letter
  to lower-case, or a shifted function key to an unshifted one.")

(defvar pre-command-hook nil
  "Normal hook run before each command is executed.
  If an unhandled error happens in running this hook,
  the function in which the error occurred is unconditionally removed, since
  otherwise the error might happen repeatedly and make Emacs nonfunctional.")

(defvar local-function-key-map nil
  "Keymap that translates key sequences to key sequences during input.
  This is used mainly for mapping key sequences into some preferred
  key events (symbols).

  The `read-key-sequence' function replaces any subsequence bound by
  `local-function-key-map' with its binding.  More precisely, when the
  active keymaps have no binding for the current key sequence but
  `local-function-key-map' binds a suffix of the sequence to a vector or
  string, `read-key-sequence' replaces the matching suffix with its
  binding, and continues with the new sequence.

  If the binding is a function, it is called with one argument (the prompt)
  and its return value (a key sequence) is used.

  The events that come from bindings in `local-function-key-map' are not
  themselves looked up in `local-function-key-map'.

  For example, suppose `local-function-key-map' binds `ESC O P' to [f1].
  Typing `ESC O P' to `read-key-sequence' would return [f1].  Typing
  `C-x ESC O P' would return [?\\C-x f1].  If [f1] were a prefix key,
  typing `ESC O P x' would return [f1 x].

  `local-function-key-map' has a separate binding for each terminal
  device.  See Info node `(elisp)Multiple Terminals'.  If you need to
  define a binding on all terminals, change `function-key-map'
  instead.  Initially, `local-function-key-map' is an empty keymap that
  has `function-key-map' as its parent on all terminal devices.")

(defvar timer-list nil
  "List of active absolute time timers in order of increasing time.")

(defvar tty-erase-char nil
  "The ERASE character as set by the user with stty.")

(defvar prefix-help-command nil
  "Command to run when `help-char' character follows a prefix key.
  This command is used only when there is no actual binding
  for that character after that prefix key.")

(defvar input-method-previous-message nil
  "When `input-method-function' is called, hold the previous echo area message.
  This variable exists because `read-event' clears the echo area
  before running the input method.  It is nil if there was no message.")

(defvar help-event-list nil
  "List of input events to recognize as meaning Help.
  These work just like the value of `help-char' (see that).

  You can customize this variable.")

(defvar double-click-fuzz nil
  "Maximum mouse movement between clicks to make a double-click.
  On window-system frames, value is the number of pixels the mouse may have
  moved horizontally or vertically between two clicks to make a double-click.
  On non window-system frames, value is interpreted in units of 1/8 characters
  instead of pixels.

  This variable is also the threshold for motion of the mouse
  to count as a drag.

  You can customize this variable.")

(defvar system-key-alist nil
  "Alist of system-specific X windows key symbols.
  Each element should have the form (N . SYMBOL) where N is the
  numeric keysym code (sans the \"system-specific\" bit 1<<28)
  and SYMBOL is its name.

  `system-key-alist' has a separate binding for each terminal device.
  See Info node `(elisp)Multiple Terminals'.")

(defvar unread-input-method-events nil
  "List of events to be processed as input by input methods.
  These events are processed after `unread-command-events', but
  before actual keyboard input.
  If there's an active input method, the events are given to
  `input-method-function'.")

(defvar echo-keystrokes 1
  "Nonzero means echo unfinished commands after this many seconds of pause.
  The value may be integer or floating point.
  If the value is zero, don't echo at all.

  You can customize this variable.")

(defvar unread-post-input-method-events nil
  "List of events to be processed as input by input methods.
  These events are processed before `unread-command-events'
  and actual keyboard input, but are not given to `input-method-function'.")

(defvar extra-keyboard-modifiers nil
  "A mask of additional modifier keys to use with every keyboard character.
  Emacs applies the modifiers of the character stored here to each keyboard
  character it reads.  For example, after evaluating the expression
      (setq extra-keyboard-modifiers ?\\C-x)
  all input characters will have the control modifier applied to them.

  Note that the character ?\\C-@, equivalent to the integer zero, does
  not count as a control character; rather, it counts as a character
  with no modifiers; thus, setting `extra-keyboard-modifiers' to zero
  cancels any modification.")

(defvar cannot-suspend nil
  "Non-nil means to always spawn a subshell instead of suspending.
  (Even if the operating system has support for stopping a process.)")

(defvar post-command-hook nil
  "Normal hook run after each command is executed.
  If an unhandled error happens in running this hook,
  the function in which the error occurred is unconditionally removed, since
  otherwise the error might happen repeatedly and make Emacs nonfunctional.")

(defvar minibuffer-message-timeout 2
  "How long to display an echo-area message when the minibuffer is active.
  If the value is not a number, such messages don't time out.")

(defvar enable-disabled-menus-and-buttons nil
  "If non-nil, don't ignore events produced by disabled menu items and tool-bar.

  Help functions bind this to allow help on disabled menu items
  and tool-bar buttons.")

(defvar input-method-function nil
  "If non-nil, the function that implements the current input method.
  It's called with one argument, a printing character that was just read.
  (That means a character with code 040...0176.)
  Typically this function uses `read-event' to read additional events.
  When it does so, it should first bind `input-method-function' to nil
  so it will not be called recursively.

  The function should return a list of zero or more events
  to be used as input.  If it wants to put back some events
  to be reconsidered, separately, by the input method,
  it can add them to the beginning of `unread-command-events'.

  The input method function can find in `input-method-previous-message'
  the previous echo area message.

  The input method function should refer to the variables
  `input-method-use-echo-area' and `input-method-exit-on-first-char'
  for guidance on what to do.")

(defvar overriding-terminal-local-map nil
  "Per-terminal keymap that overrides all other local keymaps.
  If this variable is non-nil, it is used as a keymap instead of the
  buffer's local map, and the minor mode keymaps and text property keymaps.
  It also replaces `overriding-local-map'.

  This variable is intended to let commands such as `universal-argument'
  set up a different keymap for reading the next command.

  `overriding-terminal-local-map' has a separate binding for each
  terminal device.
  See Info node `(elisp)Multiple Terminals'.")

(defvar show-help-function nil
  "If non-nil, the function that implements the display of help.
  It's called with one argument, the help string to display.")

(defvar select-active-regions nil
  "If non-nil, an active region automatically sets the primary selection.
  If the value is `only', only temporarily active regions (usually made
  by mouse-dragging or shift-selection) set the window selection.

  This takes effect only when Transient Mark mode is enabled.

  You can customize this variable.")

(defvar last-command nil
  "The last command executed.
  Normally a symbol with a function definition, but can be whatever was found
  in the keymap, or whatever the variable `this-command' was set to by that
  command.

  The value `mode-exit' is special; it means that the previous command
  read an event that told it to exit, and it did so and unread that event.
  In other words, the present command is the event that made the previous
  command exit.

  The value `kill-region' is special; it means that the previous command
  was a kill command.

  `last-command' has a separate binding for each terminal device.
  See Info node `(elisp)Multiple Terminals'.")

(defvar keyboard-translate-table nil
  "Translate table for local keyboard input, or nil.
  If non-nil, the value should be a char-table.  Each character read
  from the keyboard is looked up in this char-table.  If the value found
  there is non-nil, then it is used instead of the actual input character.

  The value can also be a string or vector, but this is considered obsolete.
  If it is a string or vector of length N, character codes N and up are left
  untranslated.  In a vector, an element which is nil means \"no translation\".

  This is applied to the characters supplied to input methods, not their
  output.  See also `translation-table-for-input'.

  This variable has a separate binding for each terminal.
  See Info node `(elisp)Multiple Terminals'.")

(defvar inhibit-local-menu-bar-menus nil
  "Non-nil means inhibit local map menu bar menus.

  You can customize this variable.")

(defvar delayed-warnings-list nil
  "List of warnings to be displayed after this command.
  Each element must be a list (TYPE MESSAGE [LEVEL [BUFFER-NAME]]),
  as per the args of `display-warning' (which see).
  If this variable is non-nil, `delayed-warnings-hook' will be run
  immediately after running `post-command-hook'.")

(defvar overriding-local-map-menu-flag nil
  "Non-nil means `overriding-local-map' applies to the menu bar.
  Otherwise, the menu bar continues to reflect the buffer's local map
  and the minor mode maps regardless of `overriding-local-map'.")

(defvar last-event-frame nil
  "The frame in which the most recently read event occurred.
  If the last event came from a keyboard macro, this is set to `macro'.")

(defvar last-nonmenu-event nil
  "Last input event in a command, except for mouse menu events.
  Mouse menus give back keys that don't look like mouse events;
  this variable holds the actual mouse event that led to the menu,
  so that you can determine whether the command was run by mouse or not.")

(defvar special-event-map nil
  "Keymap defining bindings for special events to execute at low level.")

(defvar menu-prompting true
  "Non-nil means prompt with menus when appropriate.
  This is done when reading from a keymap that has a prompt string,
  for elements that have prompt strings.
  The menu is displayed on the screen
  if X menus were enabled at configuration
  time and the previous event was a mouse click prefix key.
  Otherwise, menu prompting uses the echo area.

  You can customize this variable.")

(defvar function-key-map nil
  "The parent keymap of all `local-function-key-map' instances.
  Function key definitions that apply to all terminal devices should go
  here.  If a mapping is defined in both the current
  `local-function-key-map' binding and this variable, then the local
  definition will take precedence.")

(defvar saved-region-selection nil
  "Contents of active region prior to buffer modification.
  If `select-active-regions' is non-nil, Emacs sets this to the
  text in the region before modifying the buffer.  The next
  `deactivate-mark' call uses this to set the window selection.")

(defvar global-disable-point-adjustment nil
  "If non-nil, always suppress point adjustment.

  The default value is nil, in which case, point adjustment are
  suppressed only after special commands that set
  `disable-point-adjustment' (which see) to non-nil.")

(defvar polling-period 2
  "Interval between polling for input during Lisp execution.
  The reason for polling is to make C-g work to stop a running program.
  Polling is needed only when using X windows and SIGIO does not work.
  Polling is automatically disabled in all other cases.

  You can customize this variable.")

(defvar overriding-local-map nil
  "Keymap that overrides all other local keymaps.
  If this variable is non-nil, it is used as a keymap--replacing the
  buffer's local map, the minor mode keymaps, and char property keymaps.")

(defvar top-level nil
  "Form to evaluate when Emacs starts up.
  Useful to set before you dump a modified Emacs.")

(defvar input-decode-map nil
  "Keymap that decodes input escape sequences.
  This is used mainly for mapping ASCII function key sequences into
  real Emacs function key events (symbols).

  The `read-key-sequence' function replaces any subsequence bound by
  `input-decode-map' with its binding.  Contrary to `function-key-map',
  this map applies its rebinding regardless of the presence of an ordinary
  binding.  So it is more like `key-translation-map' except that it applies
  before `function-key-map' rather than after.

  If the binding is a function, it is called with one argument (the prompt)
  and its return value (a key sequence) is used.

  The events that come from bindings in `input-decode-map' are not
  themselves looked up in `input-decode-map'.

  This variable is keyboard-local.")

(defvar last-repeatable-command nil
  "Last command that may be repeated.
  The last command executed that was not bound to an input event.
  This is the command `repeat' will try to repeat.")

(defvar num-nonmacro-input-events nil
  "Number of input events read from the keyboard so far.
  This does not include events generated by keyboard macros.")

(defvar throw-on-input nil
  "If non-nil, any keyboard input throws to this symbol.
  The value of that variable is passed to `quit-flag' and later causes a
  peculiar kind of quitting.")

(defvar real-last-command nil
  "Same as `last-command', but never altered by Lisp code.")

(defvar help-char (int \backspace)
  "Character to recognize as meaning Help.
  When it is read, do `(eval help-form)', and display result if it's a string.
  If the value of `help-form' is nil, this char can be read normally.

  You can customize this variable.")

(defvar disable-point-adjustment nil
  "If non-nil, suppress point adjustment after executing a command.

  After a command is executed, if point is moved into a region that has
  special properties (e.g. composition, display), we adjust point to
  the boundary of the region.  But, when a command sets this variable to
  non-nil, we suppress the point adjustment.

  This variable is set to nil before reading a command, and is checked
  just after executing the command.")

(defvar debug-on-event nil
  "Enter debugger on this event.  When Emacs
  receives the special event specified by this variable, it will try to
  break into the debugger as soon as possible instead of processing the
  event normally through `special-event-map'.

  Currently, the only supported values for this
  variable are `sigusr1' and `sigusr2'.

  You can customize this variable.")

(defvar deferred-action-list nil
  "List of deferred actions to be performed at a later time.
  The precise format isn't relevant here; we just check whether it is nil.")

(defvar num-input-keys nil
  "Number of complete key sequences read as input so far.
  This includes key sequences read from keyboard macros.
  The number is effectively the number of interactive command invocations.")

(defvar command-error-function nil
  "If non-nil, function to output error messages.
  The arguments are the error data, a list of the form
   (SIGNALED-CONDITIONS . SIGNAL-DATA)
  such as just as `condition-case' would bind its variable to,
  the context (a string which normally goes at the start of the message),
  and the Lisp function within which the error was signaled.")

(defvar suggest-key-bindings true
  "Non-nil means show the equivalent key-binding when M-x command has one.
  The value can be a length of time to show the message for.
  If the value is non-nil and not a number, we wait 2 seconds.

  You can customize this variable.")

(defvar selection-inhibit-update-commands '(handle-switch-frame handle-select-window)
  "List of commands which should not update the selection.
  Normally, if `select-active-regions' is non-nil and the mark remains
  active after a command (i.e. the mark was not deactivated), the Emacs
  command loop sets the selection to the text in the region.  However,
  if the command is in this list, the selection is not updated.")

(defvar menu-bar-final-items nil
  "List of menu bar items to move to the end of the menu bar.
  The elements of the list are event types that may have menu bar bindings.")

(defvar tool-bar-separator-image-expression nil
  "Expression evaluating to the image spec for a tool-bar separator.
  This is used internally by graphical displays that do not render
  tool-bar separators natively.  Otherwise it is unused (e.g. on GTK).")

(defvar double-click-time 500
  "Maximum time between mouse clicks to make a double-click.
  Measured in milliseconds.  The value nil means disable double-click
  recognition; t means double-clicks have no time limit and are detected
  by position only.

  You can customize this variable.")

(defvar meta-prefix-char (int \)
  "Meta-prefix character code.
  Meta-foo as command input turns into this character followed by foo.

  You can customize this variable.")

(defvar auto-save-timeout 30
  "Number of seconds idle time before auto-save.
  Zero or nil means disable auto-saving due to idleness.
  After auto-saving due to this many seconds of idle time,
  Emacs also does a garbage collection if that seems to be warranted.

  You can customize this variable.")

(defvar timer-idle-list nil
  "List of active idle-time timers in order of increasing time.")

(defvar echo-area-clear-hook nil
  "Normal hook run when clearing the echo area.")

(defvar this-command nil
  "The command now being executed.
  The command can set this variable; whatever is put here
  will be in `last-command' during the following command.")

(defvar deactivate-mark nil
  "If an editing command sets this to t, deactivate the mark afterward.
  The command loop sets this to nil before each command,
  and tests the value when the command returns.
  Buffer modification stores t in this variable.")

(defvar help-form nil
  "Form to execute when character `help-char' is read.
  If the form returns a string, that string is displayed.
  If `help-form' is nil, the help char is not recognized.")

(defvar unread-command-events nil
  "List of events to be read as the command input.
  These events are processed first, before actual keyboard input.
  Events read from this list are not normally added to `this-command-keys',
  as they will already have been added once as they were read for the first time.
  An element of the form (t . EVENT) forces EVENT to be added to that list.")

(defvar menu-prompt-more-char (int \space)
  "Character to see next line of menu prompt.
  Type this character while in a menu prompt to rotate around the lines of it.")

(defvar this-original-command nil
  "The command bound to the current key sequence before remapping.
  It equals `this-command' if the original command was not remapped through
  any of the active keymaps.  Otherwise, the value of `this-command' is the
  result of looking up the original command in the active keymaps.")

(defvar key-translation-map nil
  "Keymap of key translations that can override keymaps.
  This keymap works like `function-key-map', but comes after that,
  and its non-prefix bindings override ordinary bindings.
  Another difference is that it is global rather than keyboard-local.")

;; We bypass Lanterna with System/in but utilize their setup of private mode, see UnixTerminal/enterPrivateMode:
;; (do (require '[clojure.java.shell :as sh])
;;   (sh/sh "/bin/sh" "-c" "/bin/stty -echo < /dev/tty") ;; Disables echo, leave this out for manual testing.
;;   (sh/sh "/bin/sh" "-c" "/bin/stty -icanon < /dev/tty") ;; Enable all chars for reading.
;;   (sh/sh "/bin/sh" "-c" "/bin/stty min 1 < /dev/tty")) ;; Read single chars.

;; We report our TERM as "lanterna" to allow terminal-init-lanterna to be run first, then init the real one.
;; All this has only been tested on TERM=xterm
;; input-decode-map is setup in term/xterm. We should also look in local-function-key-map
;; This interfers badly with Lanterna's get-size, occasionally locks up, needs fix.

(def ^InputStreamReader in (InputStreamReader. System/in))

(def ^:private char-buffer (atom []))
(def ^:private event-buffer (atom []))

(def ^:private recursion-depth-level (atom -1))

;; DEUCE: For reference, this is the main low level read_char function in Emacs.
;;        We don't use nmaps (or most arguments yet).
;;        We use currentTimeMillis for internal times instead of Emacs style time.
;;        Maybe totally revamped, but let's start with something "similar" to Emacs.
;;        Is normally called from read_key_sequence (C internal version) from command_loop_1.

;; /* read a character from the keyboard; call the redisplay if needed */
;; /* commandflag 0 means do not autosave, but do redisplay.
;;    -1 means do not redisplay, but do autosave.
;;    1 means do both.  */

;; /* The arguments MAPS and NMAPS are for menu prompting.
;;    MAPS is an array of keymaps;  NMAPS is the length of MAPS.

;;    PREV_EVENT is the previous input event, or nil if we are reading
;;    the first event of a key sequence (or not reading a key sequence).
;;    If PREV_EVENT is t, that is a "magic" value that says
;;    not to run input methods, but in other respects to act as if
;;    not reading a key sequence.

;;    If USED_MOUSE_MENU is non-null, then we set *USED_MOUSE_MENU to 1
;;    if we used a mouse menu to read the input, or zero otherwise.  If
;;    USED_MOUSE_MENU is null, we don't dereference it.

;;    Value is -2 when we find input on another keyboard.  A second call
;;    to read_char will read it.

;;    If END_TIME is non-null, it is a pointer to an EMACS_TIME
;;    specifying the maximum time to wait until.  If no input arrives by
;;    that time, stop waiting and return nil.

;;    Value is t if we showed a menu and the user rejected it.  */

(defn ^:private read-char [commandflag maps prev-event used-mouse-menu end-time]
  (loop []
    (when (or (nil? end-time)
              (> end-time (System/currentTimeMillis)))
      (if (.ready in)
        (.read in)
        (do (Thread/sleep 15)
            (recur))))))

(defn ^:private echo [message]
  ;; Emacs uses 2 echo areas and switches between them.
  (let [echo-area (buffer/get-buffer-create " *Echo Area 0*")]
    (if (seq message)
      (binding [buffer/*current-buffer* echo-area]
        (buffer/erase-buffer)
        (editfns/insert message))
      (binding [buffer/*current-buffer* echo-area]
        (buffer/erase-buffer)))
    (window/set-window-buffer (window/minibuffer-window) echo-area)))

(defun event-convert-list (event-desc)
  "Convert the event description list EVENT-DESC to an event type.
  EVENT-DESC should contain one base event type (a character or symbol)
  and zero or more modifier names (control, meta, hyper, super, shift, alt,
  drag, down, double or triple).  The base must be last.
  The return value is an event type (a character or symbol) which
  has the same base event type and all the specified modifiers."
  (let [[mods base] [(set (butlast event-desc)) (last event-desc)]]
    (parser/event-convert-list-internal mods base)))

(defun input-pending-p ()
  "Return t if command input is currently available with no wait.
  Actually, the value is nil only if we can be sure that no input is available;
  if there is a doubt, the value is t."
  (when (.ready in)
    true))

(defun posn-at-point (&optional pos window)
  "Return position information for buffer POS in WINDOW.
  POS defaults to point in WINDOW; WINDOW defaults to the selected window.

  Return nil if position is not visible in window.  Otherwise,
  the return value is similar to that returned by `event-start' for
  a mouse click at the upper left corner of the glyph corresponding
  to the given buffer position:
     (WINDOW AREA-OR-POS (X . Y) TIMESTAMP OBJECT POS (COL . ROW)
      IMAGE (DX . DY) (WIDTH . HEIGHT))
  The `posn-' functions access elements of such lists."
  )

(defun this-single-command-raw-keys ()
  "Return the raw events that were read for this command.
  More generally, it returns the last key sequence read, either by
  the command loop or by `read-key-sequence'.
  Unlike `this-single-command-keys', this function's value
  shows the events before all translations (except for input methods).
  The value is always a vector."
  )

(defun recent-keys ()
  "Return vector of last 300 events, not counting those from keyboard macros."
  )

(defun current-input-mode ()
  "Return information about the way Emacs currently reads keyboard input.
  The value is a list of the form (INTERRUPT FLOW META QUIT), where
    INTERRUPT is non-nil if Emacs is using interrupt-driven input; if
      nil, Emacs is using CBREAK mode.
    FLOW is non-nil if Emacs uses ^S/^Q flow control for output to the
      terminal; this does not apply if Emacs uses interrupt-driven input.
    META is t if accepting 8-bit input with 8th bit as Meta flag.
      META nil means ignoring the top bit, on the assumption it is parity.
      META is neither t nor nil if accepting 8-bit input and using
      all 8 bits as the character code.
    QUIT is the character Emacs currently uses to quit.
  The elements of this list correspond to the arguments of
  `set-input-mode'."
  )

(defun command-execute (cmd &optional record-flag keys special)
  "Execute CMD as an editor command.
  CMD must be a symbol that satisfies the `commandp' predicate.
  Optional second arg RECORD-FLAG non-nil
  means unconditionally put this command in `command-history'.
  Otherwise, that is done only if an arg is read using the minibuffer.
  The argument KEYS specifies the value to use instead of (this-command-keys)
  when reading the arguments; if it is nil, (this-command-keys) is used.
  The argument SPECIAL, if non-nil, means that this command is executing
  a special event, so ignore the prefix argument and don't clear it."
  (el/check-type 'commandp cmd)
  (try
    (echo "")
    ;; There are many more things that can happen here
    (el/setq last-event-frame (frame/selected-frame))
    (el/setq last-command-event (last @event-buffer))
    (el/setq last-nonmenu-event (last @event-buffer))
    ;; this-command-keys and this-command-keys-vector return the entire event-buffer as string or vector.
    ;; They are backed by one variable in C, this_command_keys.
    (reset! event-buffer [])
    (el/setq this-command cmd)
    (el/setq this-original-command cmd) ;; Need to handle remap
    (el/setq deactivate-mark nil)
    (buffer/set-buffer (window/window-buffer (window/selected-window)))
    (eval/run-hooks 'pre-command-hook)
    (timbre/debug (format "command-execute: %s" cmd))
    (when-not special
      (el/setq current-prefix-arg (data/symbol-value 'prefix-arg))
      (el/setq prefix-arg nil))
    (if (or (data/stringp cmd) (data/vectorp cmd))
      (macros/execute-kbd-macro cmd (when-not special (data/symbol-value 'current-prefix-arg)))
      (callint/call-interactively cmd record-flag keys))
    ;; (catch ExceptionInfo e
    ;;   (when-not (and (= 'exit (:tag (ex-data e))) (:value (ex-data e)))
    ;;     (throw e)))
    (finally
      (eval/run-hooks 'post-command-hook)
      (when (data/symbol-value 'deactivate-mark)
        (eval/funcall 'deactivate-mark))
      (el/setq this-command nil)
      (el/setq this-original-command nil)
      (el/setq last-prefix-arg (data/symbol-value 'current-prefix-arg))
      (el/setq last-command (data/symbol-value 'this-command))
      (el/setq real-last-command (data/symbol-value 'this-command)))))

(Signal/handle (Signal. "CONT")
               (proxy [SignalHandler] []
                 (handle [s] (term/resume-tty))))

(defun suspend-emacs (&optional stuffstring)
  "Stop Emacs and return to superior process.  You can resume later.
  If `cannot-suspend' is non-nil, or if the system doesn't support job
  control, run a subshell instead.

  If optional arg STUFFSTRING is non-nil, its characters are stuffed
  to be read as terminal input by Emacs's parent, after suspension.

  Before suspending, run the normal hook `suspend-hook'.
  After resumption run the normal hook `suspend-resume-hook'.

  Some operating systems cannot stop the Emacs process and resume it later.
  On such systems, Emacs starts a subshell instead of suspending."
  (interactive)
  (term/suspend-tty)
  (.invoke
   (doto (.getDeclaredMethod Signal "raise0" (into-array [Integer/TYPE]))
     (.setAccessible true)) nil (object-array [(int 20)])))

(defun recursion-depth ()
  "Return the current depth in recursive edits."
  (max @recursion-depth-level 0))

(defun read-key-sequence-vector (prompt &optional continue-echo dont-downcase-last can-return-switch-frame cmd-loop)
  "Like `read-key-sequence' but always return a vector."
  (when prompt
    (echo prompt))
  (loop [c (.read in)]
    (swap! char-buffer conj (char c))
    (let [maybe-event (object-array @char-buffer)
          decoded (keymap/lookup-key (data/symbol-value 'input-decode-map) maybe-event)]
      (if (keymap/keymapp decoded)
        (recur (.read in))
        (do (reset! char-buffer [])
            (let [event (if (data/vectorp decoded) decoded maybe-event)
                  event (or (keymap/lookup-key (data/symbol-value 'local-function-key-map) event) event)]
              (swap! event-buffer (comp vec concat) event)
              (if (keymap/keymapp (keymap/key-binding (object-array @event-buffer)))
                (recur (.read in))
                (object-array @event-buffer))))))))

(defun set-input-mode (interrupt flow meta &optional quit)
  "Set mode of reading keyboard input.
  First arg INTERRUPT non-nil means use input interrupts;
   nil means use CBREAK mode.
  Second arg FLOW non-nil means use ^S/^Q flow control for output to terminal
   (no effect except in CBREAK mode).
  Third arg META t means accept 8-bit input (for a Meta key).
   META nil means ignore the top bit, on the assumption it is parity.
   Otherwise, accept 8-bit input and don't use the top bit for Meta.
  Optional fourth arg QUIT if non-nil specifies character to use for quitting.
  See also `current-input-mode'."
  )

(defun read-key-sequence (prompt &optional continue-echo dont-downcase-last can-return-switch-frame cmd-loop)
  "Read a sequence of keystrokes and return as a string or vector.
  The sequence is sufficient to specify a non-prefix command in the
  current local and global maps.

  First arg PROMPT is a prompt string.  If nil, do not prompt specially.
  Second (optional) arg CONTINUE-ECHO, if non-nil, means this key echos
  as a continuation of the previous key.

  The third (optional) arg DONT-DOWNCASE-LAST, if non-nil, means do not
  convert the last event to lower case.  (Normally any upper case event
  is converted to lower case if the original event is undefined and the lower
  case equivalent is defined.)  A non-nil value is appropriate for reading
  a key sequence to be defined.

  A C-g typed while in this function is treated like any other character,
  and `quit-flag' is not set.

  If the key sequence starts with a mouse click, then the sequence is read
  using the keymaps of the buffer of the window clicked in, not the buffer
  of the selected window as normal.

  `read-key-sequence' drops unbound button-down events, since you normally
  only care about the click or drag events which follow them.  If a drag
  or multi-click event is unbound, but the corresponding click event would
  be bound, `read-key-sequence' turns the event into a click event at the
  drag's starting position.  This means that you don't have to distinguish
  between click and drag, double, or triple events unless you want to.

  `read-key-sequence' prefixes mouse events on mode lines, the vertical
  lines separating windows, and scroll bars with imaginary keys
  `mode-line', `vertical-line', and `vertical-scroll-bar'.

  Optional fourth argument CAN-RETURN-SWITCH-FRAME non-nil means that this
  function will process a switch-frame event if the user switches frames
  before typing anything.  If the user switches frames in the middle of a
  key sequence, or at the start of the sequence but CAN-RETURN-SWITCH-FRAME
  is nil, then the event will be put off until after the current key sequence.

  `read-key-sequence' checks `function-key-map' for function key
  sequences, where they wouldn't conflict with ordinary bindings.  See
  `function-key-map' for more details.

  The optional fifth argument CMD-LOOP, if non-nil, means
  that this key sequence is being read by something that will
  read commands one after another.  It should be nil if the caller
  will read just one key sequence."
  (apply str (map char (read-key-sequence prompt continue-echo dont-downcase-last can-return-switch-frame cmd-loop))))

(defun posn-at-x-y (x y &optional frame-or-window whole)
  "Return position information for pixel coordinates X and Y.
  By default, X and Y are relative to text area of the selected window.
  Optional third arg FRAME-OR-WINDOW non-nil specifies frame or window.
  If optional fourth arg WHOLE is non-nil, X is relative to the left
  edge of the window.

  The return value is similar to a mouse click position:
     (WINDOW AREA-OR-POS (X . Y) TIMESTAMP OBJECT POS (COL . ROW)
      IMAGE (DX . DY) (WIDTH . HEIGHT))
  The `posn-' functions access elements of such lists."
  )

(defun open-dribble-file (file)
  "Start writing all keyboard characters to a dribble file called FILE.
  If FILE is nil, close any open dribble file.
  The file will be closed when Emacs exits."
  (interactive "FOpen dribble file: "))

(defun recursive-edit ()
  "Invoke the editor command loop recursively.
  To get out of the recursive edit, a command can do `(throw 'exit nil)';
  that tells this function to return.
  Alternatively, `(throw 'exit t)' makes this function signal an error.
  This function is called by the editor initialization to begin editing."
  ;; Increases command_loop_level, calls the internal C functions:
  ;;   recursive_edit_1 -> command_loop -> command_loop_2 -> command_loop_1
  ;; Each adding some layers of condition case and other things (redisplay, buffer).
  ;; command_loop_1 is the real command loop. Calls read_key_sequence.
  (interactive)
  (let [running (atom true)]
    (try
      (swap! recursion-depth-level inc)
      (while @running
        (try
          (let [def (keymap/key-binding (read-key-sequence-vector nil))]
            (when (and def (not (keymap/keymapp def)))
              (command-execute def)))
          (catch ExceptionInfo e
            (let [{:keys [tag value]} (ex-data e)]
              (if (= 'exit tag)
                (if (nil? value)
                  (reset! running false)
                  (throw e))
                (do (editfns/message (print/error-message-string (alloc/cons tag value)))
                    (binding [*ns* (the-ns 'clojure.core)]
                      (if (= 'wrong-type-argument tag)
                        (timbre/error e)
                        (timbre/error (.getMessage e))))))))
          (catch Exception e
            ;; This is a simplification, but makes you aware of the error without tailing the log.
            ((ns-resolve 'deuce.emacs.keyboard 'echo) (.getMessage e))
            (binding [*ns* (the-ns 'clojure.core)]
              (timbre/error (el/cause e) "An error occured during the input loop")))))
      (finally
        (swap! recursion-depth-level dec)))))

(defun this-command-keys-vector ()
  "Return the key sequence that invoked this command, as a vector.
  However, if the command has called `read-key-sequence', it returns
  the last key sequence that has been read.

  See also `this-command-keys'."
  )

(defun top-level ()
  "Exit all recursive editing levels.
  This also exits all active minibuffers."
  (interactive))

(defun execute-extended-command (prefixarg)
  "Read function name, then read its arguments and call it.

  To pass a numeric argument to the command you are invoking with, specify
  the numeric argument to this command.

  Noninteractively, the argument PREFIXARG is the prefix argument to
  give to the command you invoke, if it asks for an argument."
  (interactive "P")
  (el/setq prefix-arg prefixarg)
  (command-execute
   ;; (symbol nil ((el/fun 'read-extended-command)))
   ((ns-resolve 'deuce.emacs.minibuf 'read-command) "M-x ")))

(defun discard-input ()
  "Discard the contents of the terminal input buffer.
  Also end any kbd macro being defined."
  (reset! char-buffer [])
  (reset! event-buffer [])
  (while (.ready in)
    (.read in)))

(defun reset-this-command-lengths ()
  "Make the unread events replace the last command and echo.
  Used in `universal-argument-other-key'.

  `universal-argument-other-key' rereads the event just typed.
  It then gets translated through `function-key-map'.
  The translated event has to replace the real events,
  both in the value of (this-command-keys) and in echoing.
  To achieve this, `universal-argument-other-key' calls
  `reset-this-command-lengths', which discards the record of reading
  these events the first time."
  )

(defun set-input-meta-mode (meta &optional terminal)
  "Enable or disable 8-bit input on TERMINAL.
  If META is t, Emacs will accept 8-bit input, and interpret the 8th
  bit as the Meta modifier.

  If META is nil, Emacs will ignore the top bit, on the assumption it is
  parity.

  Otherwise, Emacs will accept and pass through 8-bit input without
  specially interpreting the top bit.

  This setting only has an effect on tty terminal devices.

  Optional parameter TERMINAL specifies the tty terminal device to use.
  It may be a terminal object, a frame, or nil for the terminal used by
  the currently selected frame.

  See also `current-input-mode'."
  )

(defun this-single-command-keys ()
  "Return the key sequence that invoked this command.
  More generally, it returns the last key sequence read, either by
  the command loop or by `read-key-sequence'.
  Unlike `this-command-keys', this function's value
  does not include prefix arguments.
  The value is always a vector."
  )

(defun exit-recursive-edit ()
  "Exit from the innermost recursive edit or minibuffer."
  (interactive)
  (el/throw 'exit nil))

(defun set-quit-char (quit)
  "Specify character used for quitting.
  QUIT must be an ASCII character.

  This function only has an effect on the controlling tty of the Emacs
  process.

  See also `current-input-mode'."
  )

(defun abort-recursive-edit ()
  "Abort the command that requested this recursive edit or minibuffer input."
  (interactive)
  (el/throw 'exit true))

(defun set-input-interrupt-mode (interrupt)
  "Set interrupt mode of reading keyboard input.
  If INTERRUPT is non-nil, Emacs will use input interrupts;
  otherwise Emacs uses CBREAK mode.

  See also `current-input-mode'."
  )

(defun internal-event-symbol-parse-modifiers (symbol)
  "Parse the event symbol.  For internal use."
  )

(defun set-output-flow-control (flow &optional terminal)
  "Enable or disable ^S/^Q flow control for output to TERMINAL.
  If FLOW is non-nil, flow control is enabled and you cannot use C-s or
  C-q in key sequences.

  This setting only has an effect on tty terminals and only when
  Emacs reads input in CBREAK mode; see `set-input-interrupt-mode'.

  See also `current-input-mode'."
  )

(defun this-command-keys ()
  "Return the key sequence that invoked this command.
  However, if the command has called `read-key-sequence', it returns
  the last key sequence that has been read.
  The value is a string or a vector.

  See also `this-command-keys-vector'."
  )

(defun current-idle-time ()
  "Return the current length of Emacs idleness, or nil.
  The value when Emacs is idle is a list of three integers.  The first has
  the most significant 16 bits of the seconds, while the second has the least
  significant 16 bits.  The third integer gives the microsecond count.

  The value when Emacs is not idle is nil.

  The microsecond count is zero on systems that do not provide
  resolution finer than a second."
  )

(defun clear-this-command-keys (&optional keep-record)
  "Clear out the vector that `this-command-keys' returns.
  Also clear the record of the last 100 events, unless optional arg
  KEEP-RECORD is non-nil."
  )

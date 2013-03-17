(ns deuce.emacs.keyboard
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c])
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

(defvar echo-keystrokes nil
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

(defvar minibuffer-message-timeout nil
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

(defvar menu-prompting nil
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

(defvar polling-period nil
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

(defvar suggest-key-bindings nil
  "Non-nil means show the equivalent key-binding when M-x command has one.
  The value can be a length of time to show the message for.
  If the value is non-nil and not a number, we wait 2 seconds.

  You can customize this variable.")

(defvar selection-inhibit-update-commands nil
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

(defvar double-click-time nil
  "Maximum time between mouse clicks to make a double-click.
  Measured in milliseconds.  The value nil means disable double-click
  recognition; t means double-clicks have no time limit and are detected
  by position only.

  You can customize this variable.")

(defvar meta-prefix-char (int \)
  "Meta-prefix character code.
  Meta-foo as command input turns into this character followed by foo.

  You can customize this variable.")

(defvar auto-save-timeout nil
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

(defvar menu-prompt-more-char nil
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

(defun event-convert-list (event-desc)
  "Convert the event description list EVENT-DESC to an event type.
  EVENT-DESC should contain one base event type (a character or symbol)
  and zero or more modifier names (control, meta, hyper, super, shift, alt,
  drag, down, double or triple).  The base must be last.
  The return value is an event type (a character or symbol) which
  has the same base event type and all the specified modifiers."
  )

(defun input-pending-p ()
  "Return t if command input is currently available with no wait.
  Actually, the value is nil only if we can be sure that no input is available;
  if there is a doubt, the value is t."
  )

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
  )

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
  )

(defun recursion-depth ()
  "Return the current depth in recursive edits."
  0)

(defun read-key-sequence-vector (prompt &optional continue-echo dont-downcase-last can-return-switch-frame cmd-loop)
  "Like `read-key-sequence' but always return a vector."
  )

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
  )

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
  )

(defun recursive-edit ()
  "Invoke the editor command loop recursively.
  To get out of the recursive edit, a command can do `(throw 'exit nil)';
  that tells this function to return.
  Alternatively, `(throw 'exit t)' makes this function signal an error.
  This function is called by the editor initialization to begin editing."
  )

(defun this-command-keys-vector ()
  "Return the key sequence that invoked this command, as a vector.
  However, if the command has called `read-key-sequence', it returns
  the last key sequence that has been read.

  See also `this-command-keys'."
  )

(defun top-level ()
  "Exit all recursive editing levels.
  This also exits all active minibuffers."
  )

(defun execute-extended-command (prefixarg)
  "Read function name, then read its arguments and call it.

  To pass a numeric argument to the command you are invoking with, specify
  the numeric argument to this command.

  Noninteractively, the argument PREFIXARG is the prefix argument to
  give to the command you invoke, if it asks for an argument."
  )

(defun discard-input ()
  "Discard the contents of the terminal input buffer.
  Also end any kbd macro being defined."
  )

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
  )

(defun set-quit-char (quit)
  "Specify character used for quitting.
  QUIT must be an ASCII character.

  This function only has an effect on the controlling tty of the Emacs
  process.

  See also `current-input-mode'."
  )

(defun abort-recursive-edit ()
  "Abort the command that requested this recursive edit or minibuffer input."
  )

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

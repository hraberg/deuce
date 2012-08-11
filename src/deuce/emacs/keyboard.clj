(ns
 deuce.emacs.keyboard
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

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
  )

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

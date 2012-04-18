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
  POS defaults to point in WINDOW; WINDOW defaults to the selected window."
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
  control, run a subshell instead."
  )

(defun recursion-depth ()
  "Return the current depth in recursive edits."
  )

(defun read-key-sequence-vector (prompt &optional continue-echo dont-downcase-last can-return-switch-frame command-loop)
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

(defun read-key-sequence (prompt &optional continue-echo dont-downcase-last can-return-switch-frame command-loop)
  "Read a sequence of keystrokes and return as a string or vector.
  The sequence is sufficient to specify a non-prefix command in the
  current local and global maps."
  )

(defun posn-at-x-y (x y &optional frame-or-window whole)
  "Return position information for pixel coordinates X and Y.
  By default, X and Y are relative to text area of the selected window.
  Optional third arg FRAME-OR-WINDOW non-nil specifies frame or window.
  If optional fourth arg WHOLE is non-nil, X is relative to the left
  edge of the window."
  )

(defun open-dribble-file (file)
  "Start writing all keyboard characters to a dribble file called FILE.
  If FILE is nil, close any open dribble file."
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
  the last key sequence that has been read."
  )

(defun top-level ()
  "Exit all recursive editing levels.
  This also exits all active minibuffers."
  )

(defun execute-extended-command (prefixarg)
  "Read function name, then read its arguments and call it."
  )

(defun discard-input ()
  "Discard the contents of the terminal input buffer.
  Also end any kbd macro being defined."
  )

(defun reset-this-command-lengths ()
  "Make the unread events replace the last command and echo.
  Used in `universal-argument-other-key'."
  )

(defun set-input-meta-mode (meta &optional terminal)
  "Enable or disable 8-bit input on TERMINAL.
  If META is t, Emacs will accept 8-bit input, and interpret the 8th
  bit as the Meta modifier."
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
  QUIT must be an ASCII character."
  )

(defun abort-recursive-edit ()
  "Abort the command that requested this recursive edit or minibuffer input."
  )

(defun set-input-interrupt-mode (interrupt)
  "Set interrupt mode of reading keyboard input.
  If INTERRUPT is non-nil, Emacs will use input interrupts;
  otherwise Emacs uses CBREAK mode."
  )

(defun internal-event-symbol-parse-modifiers (symbol)
  "Parse the event symbol.  For internal use."
  )

(defun set-output-flow-control (flow &optional terminal)
  "Enable or disable ^S/^Q flow control for output to TERMINAL.
  If FLOW is non-nil, flow control is enabled and you cannot use C-s or
  C-q in key sequences."
  )

(defun this-command-keys ()
  "Return the key sequence that invoked this command.
  However, if the command has called `read-key-sequence', it returns
  the last key sequence that has been read.
  The value is a string or a vector."
  )

(defun current-idle-time ()
  "Return the current length of Emacs idleness, or nil.
  The value when Emacs is idle is a list of three integers.  The first has
  the most significant 16 bits of the seconds, while the second has the least
  significant 16 bits.  The third integer gives the microsecond count."
  )

(defun clear-this-command-keys (&optional keep-record)
  "Clear out the vector that `this-command-keys' returns.
  Also clear the record of the last 100 events, unless optional arg
  KEEP-RECORD is non-nil."
  )

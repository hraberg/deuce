(ns emacs.keyboard (use [deuce.core]) (:refer-clojure :only []))

(defun event-convert-list (event-desc)
  "Convert the event description list EVENT-DESC to an event type.\nEVENT-DESC should contain one base event type (a character or symbol)\nand zero or more modifier names (control, meta, hyper, super, shift, alt,\ndrag, down, double or triple).  The base must be last.\nThe return value is an event type (a character or symbol) which\n"
  )

(defun input-pending-p ()
  "Return t if command input is currently available with no wait.\nActually, the value is nil only if we can be sure that no input is available;\n"
  )

(defun posn-at-point (&optional pos window)
  "Return position information for buffer POS in WINDOW.\nPOS defaults to point in WINDOW; WINDOW defaults to the selected window."
  )

(defun recent-keys ()
  )

(defun current-input-mode ()
  "Return information about the way Emacs currently reads keyboard input.\nThe value is a list of the form (INTERRUPT FLOW META QUIT), where\n  INTERRUPT is non-nil if Emacs is using interrupt-driven input; if\n    nil, Emacs is using CBREAK mode.\n  FLOW is non-nil if Emacs uses ^S/^Q flow control for output to the\n    terminal; this does not apply if Emacs uses interrupt-driven input.\n  META is t if accepting 8-bit input with 8th bit as Meta flag.\n    META nil means ignoring the top bit, on the assumption it is parity.\n    META is neither t nor nil if accepting 8-bit input and using\n    all 8 bits as the character code.\n  QUIT is the character Emacs currently uses to quit.\nThe elements of this list correspond to the arguments of\n"
  )

(defun command-execute (cmd &optional record-flag keys special)
  "Execute CMD as an editor command.\nCMD must be a symbol that satisfies the `commandp' predicate.\nOptional second arg RECORD-FLAG non-nil\nmeans unconditionally put this command in `command-history'.\nOtherwise, that is done only if an arg is read using the minibuffer.\nThe argument KEYS specifies the value to use instead of (this-command-keys)\nwhen reading the arguments; if it is nil, (this-command-keys) is used.\nThe argument SPECIAL, if non-nil, means that this command is executing\n"
  )

(defun recursion-depth ()
  "Return the current depth in recursive edits.exit-recursive-edit is an interactive built-in function in `C source\ncode'."
  )

(defun read-key-sequence-vector (prompt &optional continue-echo dont-downcase-last can-return-switch-frame command-loop)
  "Like `read-key-sequence' but always return a vector.previous-single-char-property-change is a built-in function in `C\nsource code'."
  )

(defun set-input-mode (interrupt flow meta &optional quit)
  "Set mode of reading keyboard input.\nFirst arg INTERRUPT non-nil means use input interrupts;\n nil means use CBREAK mode.\nSecond arg FLOW non-nil means use ^S/^Q flow control for output to terminal\n (no effect except in CBREAK mode).\nThird arg META t means accept 8-bit input (for a Meta key).\n META nil means ignore the top bit, on the assumption it is parity.\n Otherwise, accept 8-bit input and don't use the top bit for Meta.\nOptional fourth arg QUIT if non-nil specifies character to use for quitting.\n"
  )

(defun read-key-sequence (prompt &optional continue-echo dont-downcase-last can-return-switch-frame command-loop)
  "Read a sequence of keystrokes and return as a string or vector.\nThe sequence is sufficient to specify a non-prefix command in the\ncurrent local and global maps."
  )

(defun posn-at-x-y (x y &optional frame-or-window whole)
  "Return position information for pixel coordinates X and Y.\nBy default, X and Y are relative to text area of the selected window.\nOptional third arg FRAME-OR-WINDOW non-nil specifies frame or window.\nIf optional fourth arg WHOLE is non-nil, X is relative to the left\nedge of the window."
  )

(defun this-command-keys-vector ()
  "Return the key sequence that invoked this command, as a vector.\nHowever, if the command has called `read-key-sequence', it returns\nthe last key sequence that has been read."
  )

(defun discard-input ()
  "Discard the contents of the terminal input buffer.\n"
  )

(defun reset-this-command-lengths ()
  "Make the unread events replace the last command and echo.\nUsed in `universal-argument-other-key'."
  )

(defun set-input-meta-mode (meta &optional terminal)
  "Enable or disable 8-bit input on TERMINAL.\nIf META is t, Emacs will accept 8-bit input, and interpret the 8th\nbit as the Meta modifier."
  )

(defun this-single-command-keys ()
  "Return the key sequence that invoked this command.\nMore generally, it returns the last key sequence read, either by\nthe command loop or by `read-key-sequence'.\nUnlike `this-command-keys', this function's value\ndoes not include prefix arguments.\n"
  )

(defun set-quit-char (quit)
  "Specify character used for quitting.\nQUIT must be an ASCII character."
  )

(defun set-input-interrupt-mode (interrupt)
  "Set interrupt mode of reading keyboard input.\nIf INTERRUPT is non-nil, Emacs will use input interrupts;\notherwise Emacs uses CBREAK mode."
  )

(defun set-output-flow-control (flow &optional terminal)
  "Enable or disable ^S/^Q flow control for output to TERMINAL.\nIf FLOW is non-nil, flow control is enabled and you cannot use C-s or\nC-q in key sequences."
  )

(defun this-command-keys ()
  "Return the key sequence that invoked this command.\nHowever, if the command has called `read-key-sequence', it returns\nthe last key sequence that has been read.\nThe value is a string or a vector."
  )

(defun current-idle-time ()
  "Return the current length of Emacs idleness, or nil.\nThe value when Emacs is idle is a list of three integers.  The first has\nthe most significant 16 bits of the seconds, while the second has the least\nsignificant 16 bits.  The third integer gives the microsecond count."
  )

(defun clear-this-command-keys (&optional keep-record)
  "Clear out the vector that `this-command-keys' returns.\nAlso clear the record of the last 100 events, unless optional arg\n"
  )

(ns
 emacs.minibuf
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude [read-string]))

(defun minibuffer-depth ()
  "Return current depth of activations of minibuffer, a nonnegative integer."
  )

(defun test-completion (string collection &optional predicate)
  "Return non-nil if STRING is a valid completion.
  Takes the same arguments as `all-completions' and `try-completion'.
  If COLLECTION is a function, it is called with three arguments:
  the values STRING, PREDICATE and `lambda'."
  )

(defun completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Read a string in the minibuffer, with completion.
  PROMPT is a string to prompt with; normally it ends in a colon and a space.
  COLLECTION can be a list of strings, an alist, an obarray or a hash table.
  COLLECTION can also be a function to do the completion itself.
  PREDICATE limits completion to a subset of COLLECTION.
  See `try-completion' and `all-completions' for more details
   on completion, COLLECTION, and PREDICATE."
  )

(defun read-from-minibuffer (prompt &optional initial-contents keymap read hist default-value inherit-input-method)
  "Read a string from the minibuffer, prompting with string PROMPT.
  The optional second arg INITIAL-CONTENTS is an obsolete alternative to
    DEFAULT-VALUE.  It normally should be nil in new code, except when
    HIST is a cons.  It is discussed in more detail below.
  Third arg KEYMAP is a keymap to use whilst reading;
    if omitted or nil, the default is `minibuffer-local-map'.
  If fourth arg READ is non-nil, then interpret the result as a Lisp object
    and return that object:
    in other words, do `(car (read-from-string INPUT-STRING))'
  Fifth arg HIST, if non-nil, specifies a history list and optionally
    the initial position in the list.  It can be a symbol, which is the
    history list variable to use, or it can be a cons cell
    (HISTVAR . HISTPOS).  In that case, HISTVAR is the history list variable
    to use, and HISTPOS is the initial position for use by the minibuffer
    history commands.  For consistency, you should also specify that
    element of the history as the value of INITIAL-CONTENTS.  Positions
    are counted starting from 1 at the beginning of the list.
  Sixth arg DEFAULT-VALUE is the default value or the list of default values.
    If non-nil, it is available for history commands, and as the value
    (or the first element of the list of default values) to return
    if the user enters the empty string.  But, unless READ is non-nil,
    `read-from-minibuffer' does NOT return DEFAULT-VALUE if the user enters
    empty input!  It returns the empty string.
  Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
   the current input method and the setting of `enable-multibyte-characters'.
  If the variable `minibuffer-allow-text-properties' is non-nil,
   then the string which is returned includes whatever text properties
   were present in the minibuffer.  Otherwise the value has no text properties."
  )

(defun assoc-string (key list &optional case-fold)
  "Like `assoc' but specifically for strings (and symbols)."
  )

(defun read-no-blanks-input (prompt &optional initial inherit-input-method)
  "Read a string from the terminal, not allowing blanks.
  Prompt with PROMPT.  Whitespace terminates the input.  If INITIAL is
  non-nil, it should be a string, which is used as initial input, with
  point positioned at the end, so that SPACE will accept the input.
  (Actually, INITIAL can also be a cons of a string and an integer.
  Such values are treated as in `read-from-minibuffer', but are normally
  not useful in this function.)
  Third arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
  the current input method and the setting of`enable-multibyte-characters'."
  )

(defun minibuffer-prompt ()
  "Return the prompt string of the currently-active minibuffer.
  If no minibuffer is active, return nil."
  )

(defun read-command (prompt &optional default-value)
  "Read the name of a command and return as a symbol.
  Prompt with PROMPT.  By default, return DEFAULT-VALUE or its first element
  if it is a list."
  )

(defun try-completion (string collection &optional predicate)
  "Return common substring of all completions of STRING in COLLECTION.
  Test each possible completion specified by COLLECTION
  to see if it begins with STRING.  The possible completions may be
  strings or symbols.  Symbols are converted to strings before testing,
  see `symbol-name'.
  All that match STRING are compared together; the longest initial sequence
  common to all these matches is the return value.
  If there is no match at all, the return value is nil.
  For a unique match which is exact, the return value is t."
  )

(defun eval-minibuffer (prompt &optional initial-contents)
  "Return value of Lisp expression read using the minibuffer.
  Prompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS
  is a string to insert in the minibuffer before reading.
  (INITIAL-CONTENTS can also be a cons of a string and an integer.
  Such arguments are used as in `read-from-minibuffer'.)"
  )

(defun read-string (prompt &optional initial-input history default-value inherit-input-method)
  "Read a string from the minibuffer, prompting with string PROMPT.
  If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
    This argument has been superseded by DEFAULT-VALUE and should normally
    be nil in new code.  It behaves as in `read-from-minibuffer'.  See the
    documentation string of that function for details.
  The third arg HISTORY, if non-nil, specifies a history list
    and optionally the initial position in the list.
  See `read-from-minibuffer' for details of HISTORY argument.
  Fourth arg DEFAULT-VALUE is the default value or the list of default values.
   If non-nil, it is used for history commands, and as the value (or the first
   element of the list of default values) to return if the user enters the
   empty string.
  Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
   the current input method and the setting of `enable-multibyte-characters'."
  )

(defun minibuffer-prompt-end ()
  "Return the buffer position of the end of the minibuffer prompt.
  Return (point-min) if current buffer is not a minibuffer."
  )

(defun set-minibuffer-window (window)
  "Specify which minibuffer window to use for the minibuffer.
  This affects where the minibuffer is displayed if you put text in it
  without invoking the usual minibuffer commands."
  )

(defun minibufferp (&optional buffer)
  "Return t if BUFFER is a minibuffer.
  No argument or nil as argument means use current buffer as BUFFER.
  BUFFER can be a buffer or a buffer name."
  )

(defun internal-complete-buffer (string predicate flag)
  "Perform completion on buffer names.
  If the argument FLAG is nil, invoke `try-completion', if it's t, invoke
  `all-completions', otherwise invoke `test-completion'."
  )

(defun all-completions (string collection &optional predicate)
  "Search for partial matches to STRING in COLLECTION.
  Test each of the possible completions specified by COLLECTION
  to see if it begins with STRING.  The possible completions may be
  strings or symbols.  Symbols are converted to strings before testing,
  see `symbol-name'.
  The value is a list of all the possible completions that match STRING."
  )

(defun read-buffer (prompt &optional def require-match)
  "Read the name of a buffer and return as a string.
  Prompt with PROMPT.
  Optional second arg DEF is value to return if user enters an empty line.
   If DEF is a list of default values, return its first element.
  Optional third arg REQUIRE-MATCH determines whether non-existing
   buffer names are allowed.  It has the same meaning as the
   REQUIRE-MATCH argument of `completing-read'.
  The argument PROMPT should be a string ending with a colon and a space.
  If `read-buffer-completion-ignore-case' is non-nil, completion ignores
  case while reading the buffer name.
  If `read-buffer-function' is non-nil, this works by calling it as a
  function, instead of the usual behavior."
  )

(defun read-minibuffer (prompt &optional initial-contents)
  "Return a Lisp object read using the minibuffer, unevaluated.
  Prompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS
  is a string to insert in the minibuffer before reading.
  (INITIAL-CONTENTS can also be a cons of a string and an integer.
  Such arguments are used as in `read-from-minibuffer'.)"
  )

(defun minibuffer-contents-no-properties ()
  "Return the user input in a minibuffer as a string, without text-properties.
  If the current buffer is not a minibuffer, return its entire contents."
  )

(defun minibuffer-contents ()
  "Return the user input in a minibuffer as a string.
  If the current buffer is not a minibuffer, return its entire contents."
  )

(defun read-variable (prompt &optional default-value)
  "Read the name of a user variable and return it as a symbol.
  Prompt with PROMPT.  By default, return DEFAULT-VALUE or its first element
  if it is a list.
  A user variable is one for which `user-variable-p' returns non-nil."
  )

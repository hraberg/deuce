(ns emacs.minibuf (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun minibuffer-depth ()
  "Return current depth of activations of minibuffer, a nonnegative integer.internal-lisp-face-attribute-values is a built-in function in `C\nsource code'."
  )

(defun test-completion (string collection &optional predicate)
  "Return non-nil if STRING is a valid completion.\nTakes the same arguments as `all-completions' and `try-completion'.\nIf COLLECTION is a function, it is called with three arguments:\nthe values STRING, PREDICATE and `lambda'.top-level is an interactive built-in function in `C source code'."
  )

(defun completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Read a string in the minibuffer, with completion.\nPROMPT is a string to prompt with; normally it ends in a colon and a space.\nCOLLECTION can be a list of strings, an alist, an obarray or a hash table.\nCOLLECTION can also be a function to do the completion itself.\nPREDICATE limits completion to a subset of COLLECTION.\nSee `try-completion' and `all-completions' for more details\n on completion, COLLECTION, and PREDICATE."
  )

(defun read-from-minibuffer (prompt &optional initial-contents keymap read hist default-value inherit-input-method)
  "Read a string from the minibuffer, prompting with string PROMPT.\nThe optional second arg INITIAL-CONTENTS is an obsolete alternative to\n  DEFAULT-VALUE.  It normally should be nil in new code, except when\n  HIST is a cons.  It is discussed in more detail below.\nThird arg KEYMAP is a keymap to use whilst reading;\n  if omitted or nil, the default is `minibuffer-local-map'.\nIf fourth arg READ is non-nil, then interpret the result as a Lisp object\n  and return that object:\n  in other words, do `(car (read-from-string INPUT-STRING))'\nFifth arg HIST, if non-nil, specifies a history list and optionally\n  the initial position in the list.  It can be a symbol, which is the\n  history list variable to use, or it can be a cons cell\n  (HISTVAR . HISTPOS).  In that case, HISTVAR is the history list variable\n  to use, and HISTPOS is the initial position for use by the minibuffer\n  history commands.  For consistency, you should also specify that\n  element of the history as the value of INITIAL-CONTENTS.  Positions\n  are counted starting from 1 at the beginning of the list.\nSixth arg DEFAULT-VALUE is the default value or the list of default values.\n  If non-nil, it is available for history commands, and as the value\n  (or the first element of the list of default values) to return\n  if the user enters the empty string.  But, unless READ is non-nil,\n  `read-from-minibuffer' does NOT return DEFAULT-VALUE if the user enters\n  empty input!  It returns the empty string.\nSeventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits\n the current input method and the setting of `enable-multibyte-characters'.\nIf the variable `minibuffer-allow-text-properties' is non-nil,\n then the string which is returned includes whatever text properties\n were present in the minibuffer.  Otherwise the value has no text properties."
  )

(defun assoc-string (key list &optional case-fold)
  "Like `assoc' but specifically for strings (and symbols)."
  )

(defun read-no-blanks-input (prompt &optional initial inherit-input-method)
  "Read a string from the terminal, not allowing blanks.\nPrompt with PROMPT.  Whitespace terminates the input.  If INITIAL is\nnon-nil, it should be a string, which is used as initial input, with\npoint positioned at the end, so that SPACE will accept the input.\n(Actually, INITIAL can also be a cons of a string and an integer.\nSuch values are treated as in `read-from-minibuffer', but are normally\nnot useful in this function.)\nThird arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits\n"
  )

(defun minibuffer-prompt ()
  "Return the prompt string of the currently-active minibuffer.\n"
  )

(defun read-command (prompt &optional default-value)
  "Read the name of a command and return as a symbol.\nPrompt with PROMPT.  By default, return DEFAULT-VALUE or its first element\n"
  )

(defun try-completion (string collection &optional predicate)
  "Return common substring of all completions of STRING in COLLECTION.\nTest each possible completion specified by COLLECTION\nto see if it begins with STRING.  The possible completions may be\nstrings or symbols.  Symbols are converted to strings before testing,\nsee `symbol-name'.\nAll that match STRING are compared together; the longest initial sequence\ncommon to all these matches is the return value.\nIf there is no match at all, the return value is nil.\nFor a unique match which is exact, the return value is t."
  )

(defun eval-minibuffer (prompt &optional initial-contents)
  "Return value of Lisp expression read using the minibuffer.\nPrompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS\nis a string to insert in the minibuffer before reading.\n(INITIAL-CONTENTS can also be a cons of a string and an integer.\n"
  )

(defun read-string (prompt &optional initial-input history default-value inherit-input-method)
  "Read a string from the minibuffer, prompting with string PROMPT.\nIf non-nil, second arg INITIAL-INPUT is a string to insert before reading.\n  This argument has been superseded by DEFAULT-VALUE and should normally\n  be nil in new code.  It behaves as in `read-from-minibuffer'.  See the\n  documentation string of that function for details.\nThe third arg HISTORY, if non-nil, specifies a history list\n  and optionally the initial position in the list.\nSee `read-from-minibuffer' for details of HISTORY argument.\nFourth arg DEFAULT-VALUE is the default value or the list of default values.\n If non-nil, it is used for history commands, and as the value (or the first\n element of the list of default values) to return if the user enters the\n empty string.\nFifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits\n the current input method and the setting of `enable-multibyte-characters'.call-last-kbd-macro is an interactive built-in function in `C source\ncode'."
  )

(defun minibuffer-prompt-end ()
  "Return the buffer position of the end of the minibuffer prompt.\n"
  )

(defun set-minibuffer-window (window)
  "Specify which minibuffer window to use for the minibuffer.\nThis affects where the minibuffer is displayed if you put text in it\nwithout invoking the usual minibuffer commands.previous-char-property-change is a built-in function in `C source\ncode'."
  )

(defun minibufferp (&optional buffer)
  "Return t if BUFFER is a minibuffer.\nNo argument or nil as argument means use current buffer as BUFFER.\nBUFFER can be a buffer or a buffer name.catch is a special form in `C source code'."
  )

(defun internal-complete-buffer (string predicate flag)
  "Perform completion on buffer names.\nIf the argument FLAG is nil, invoke `try-completion', if it's t, invoke\n`all-completions', otherwise invoke `test-completion'."
  )

(defun all-completions (string collection &optional predicate)
  "Search for partial matches to STRING in COLLECTION.\nTest each of the possible completions specified by COLLECTION\nto see if it begins with STRING.  The possible completions may be\nstrings or symbols.  Symbols are converted to strings before testing,\nsee `symbol-name'.\nThe value is a list of all the possible completions that match STRING."
  )

(defun read-buffer (prompt &optional def require-match)
  "Read the name of a buffer and return as a string.\nPrompt with PROMPT.\nOptional second arg DEF is value to return if user enters an empty line.\n If DEF is a list of default values, return its first element.\nOptional third arg REQUIRE-MATCH determines whether non-existing\n buffer names are allowed.  It has the same meaning as the\n REQUIRE-MATCH argument of `completing-read'.\nThe argument PROMPT should be a string ending with a colon and a space.\nIf `read-buffer-completion-ignore-case' is non-nil, completion ignores\ncase while reading the buffer name.\nIf `read-buffer-function' is non-nil, this works by calling it as a\n"
  )

(defun read-minibuffer (prompt &optional initial-contents)
  "Return a Lisp object read using the minibuffer, unevaluated.\nPrompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS\nis a string to insert in the minibuffer before reading.\n(INITIAL-CONTENTS can also be a cons of a string and an integer.\nSuch arguments are used as in `read-from-minibuffer'.)save-restriction is a special form in `C source code'."
  )

(defun minibuffer-contents ()
  "Return the user input in a minibuffer as a string.\n"
  )

(defun read-variable (prompt &optional default-value)
  "Read the name of a user variable and return it as a symbol.\nPrompt with PROMPT.  By default, return DEFAULT-VALUE or its first element\nif it is a list.\n"
  )

(ns deuce.emacs.minibuf
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c]
            [deuce.emacs-lisp :as el]
            [deuce.emacs-lisp.cons :refer [ICons car] :as cons]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.fns :as fns])
  (:refer-clojure :exclude [read-string]))

(defvar minibuffer-history-variable nil
  "History list symbol to add minibuffer values to.
  Each string of minibuffer input, as it appears on exit from the minibuffer,
  is added with
    (set minibuffer-history-variable
    (cons STRING (symbol-value minibuffer-history-variable)))")

(defvar read-expression-history nil
  "A history list for arguments that are Lisp expressions to evaluate.
  For example, `eval-expression' uses this.")

(defvar completion-regexp-list nil
  "List of regexps that should restrict possible completions.
  The basic completion functions only consider a completion acceptable
  if it matches all regular expressions in this list, with
  `case-fold-search' bound to the value of `completion-ignore-case'.
  See Info node `(elisp)Basic Completion', for a description of these
  functions.")

(defvar minibuffer-exit-hook nil
  "Normal hook run just after exit from minibuffer.")

(defvar minibuffer-completing-file-name nil
  "Non-nil means completing file names.")

(defvar enable-recursive-minibuffers nil
  "Non-nil means to allow minibuffer commands while in the minibuffer.
  This variable makes a difference whenever the minibuffer window is active.

  You can customize this variable.")

(defvar minibuffer-setup-hook nil
  "Normal hook run just after entry to minibuffer.")

(defvar minibuffer-prompt-properties nil
  "Text properties that are added to minibuffer prompts.
  These are in addition to the basic `field' property, and stickiness
  properties.

  You can customize this variable.")

(defvar read-buffer-completion-ignore-case nil
  "Non-nil means completion ignores case when reading a buffer name.

  You can customize this variable.")

(defvar minibuffer-history-position nil
  "Current position of redoing in the history list.")

(defvar read-buffer-function nil
  "If this is non-nil, `read-buffer' does its work by calling this function.
  The function is called with the arguments passed to `read-buffer'.

  You can customize this variable.")

(defvar history-delete-duplicates nil
  "Non-nil means to delete duplicates in history.
  If set to t when adding a new history element, all previous identical
  elements are deleted from the history list.

  You can customize this variable.")

(defvar minibuffer-completion-predicate nil
  "Within call to `completing-read', this holds the PREDICATE argument.")

(defvar completion-ignore-case nil
  "Non-nil means don't consider case significant in completion.
  For file-name completion, `read-file-name-completion-ignore-case'
  controls the behavior, rather than this variable.
  For buffer name completion, `read-buffer-completion-ignore-case'
  controls the behavior, rather than this variable.")

(defvar minibuffer-help-form nil
  "Value that `help-form' takes on inside the minibuffer.")

(defvar minibuffer-allow-text-properties nil
  "Non-nil means `read-from-minibuffer' should not discard text properties.
  This also affects `read-string', but it does not affect `read-minibuffer',
  `read-no-blanks-input', or any of the functions that do minibuffer input
  with completion; they always discard text properties.")

(defvar read-expression-map nil
  "Minibuffer keymap used for reading Lisp expressions.")

(defvar history-length nil
  "Maximum length of history lists before truncation takes place.
  A number means truncate to that length; truncation deletes old
  elements, and is done just after inserting a new element.
  A value of t means no truncation.

  This variable only affects history lists that don't specify their own
  maximum lengths.  Setting the `history-length' property of a history
  variable overrides this default.

  You can customize this variable.")

(defvar minibuffer-auto-raise nil
  "Non-nil means entering the minibuffer raises the minibuffer's frame.
  Some uses of the echo area also raise that frame (since they use it too).

  You can customize this variable.")

(defvar minibuffer-completion-table nil
  "Alist or obarray used for completion in the minibuffer.
  This becomes the ALIST argument to `try-completion' and `all-completions'.
  The value can also be a list of strings or a hash table.

  The value may alternatively be a function, which is given three arguments:
    STRING, the current buffer contents;
    PREDICATE, the predicate for filtering possible matches;
    CODE, which says what kind of things to do.
  CODE can be nil, t or `lambda':
    nil    -- return the best completion of STRING, or nil if there is none.
    t      -- return a list of all possible completions of STRING.
    lambda -- return t if STRING is a valid completion as it stands.")

(defvar history-add-new-input nil
  "Non-nil means to add new elements in history.
  If set to nil, minibuffer reading functions don't add new elements to the
  history list, so it is possible to do this afterwards by calling
  `add-to-history' explicitly.")

(defvar minibuffer-completion-confirm nil
  "Whether to demand confirmation of completion before exiting minibuffer.
  If nil, confirmation is not required.
  If the value is `confirm', the user may exit with an input that is not
   a valid completion alternative, but Emacs asks for confirmation.
  If the value is `confirm-after-completion', the user may exit with an
   input that is not a valid completion alternative, but Emacs asks for
   confirmation if the user submitted the input right after any of the
   completion commands listed in `minibuffer-confirm-exit-commands'.")

(defun minibuffer-depth ()
  "Return current depth of activations of minibuffer, a nonnegative integer."
  )

(declare filter-completions)

(defun test-completion (string collection &optional predicate)
  "Return non-nil if STRING is a valid completion.
  Takes the same arguments as `all-completions' and `try-completion'.
  If COLLECTION is a function, it is called with three arguments:
  the values STRING, PREDICATE and `lambda'."
  (when (some #{string} (filter-completions collection predicate))
    true))

(defun completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Read a string in the minibuffer, with completion.
  PROMPT is a string to prompt with; normally it ends in a colon and a space.
  COLLECTION can be a list of strings, an alist, an obarray or a hash table.
  COLLECTION can also be a function to do the completion itself.
  PREDICATE limits completion to a subset of COLLECTION.
  See `try-completion' and `all-completions' for more details
   on completion, COLLECTION, and PREDICATE.

  REQUIRE-MATCH can take the following values:
  - t means that the user is not allowed to exit unless
    the input is (or completes to) an element of COLLECTION or is null.
  - nil means that the user can exit with any input.
  - `confirm' means that the user can exit with any input, but she needs
    to confirm her choice if the input is not an element of COLLECTION.
  - `confirm-after-completion' means that the user can exit with any
    input, but she needs to confirm her choice if she called
    `minibuffer-complete' right before `minibuffer-complete-and-exit'
    and the input is not an element of COLLECTION.
  - anything else behaves like t except that typing RET does not exit if it
    does non-null completion.

  If the input is null, `completing-read' returns DEF, or the first element
  of the list of default values, or an empty string if DEF is nil,
  regardless of the value of REQUIRE-MATCH.

  If INITIAL-INPUT is non-nil, insert it in the minibuffer initially,
    with point positioned at the end.
    If it is (STRING . POSITION), the initial input is STRING, but point
    is placed at _zero-indexed_ position POSITION in STRING.  (*Note*
    that this is different from `read-from-minibuffer' and related
    functions, which use one-indexing for POSITION.)  This feature is
    deprecated--it is best to pass nil for INITIAL-INPUT and supply the
    default value DEF instead.  The user can yank the default value into
    the minibuffer easily using M-x next-history-element.

  HIST, if non-nil, specifies a history list and optionally the initial
    position in the list.  It can be a symbol, which is the history list
    variable to use, or it can be a cons cell (HISTVAR . HISTPOS).  In
    that case, HISTVAR is the history list variable to use, and HISTPOS
    is the initial position (the position in the list used by the
    minibuffer history commands).  For consistency, you should also
    specify that element of the history as the value of
    INITIAL-INPUT.  (This is the only case in which you should use
    INITIAL-INPUT instead of DEF.)  Positions are counted starting from
    1 at the beginning of the list.  The variable `history-length'
    controls the maximum length of a history list.

  DEF, if non-nil, is the default value or the list of default values.

  If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits
    the current input method and the setting of `enable-multibyte-characters'.

  Completion ignores case if the ambient value of
    `completion-ignore-case' is non-nil.

  See also `completing-read-function'."
  )

(defun read-from-minibuffer (prompt &optional initial-contents keymap read hist default-value inherit-input-method)
  "Read a string from the minibuffer, prompting with string PROMPT.
  The optional second arg INITIAL-CONTENTS is an obsolete alternative to
    DEFAULT-VALUE.  It normally should be nil in new code, except when
    HIST is a cons.  It is discussed in more detail below.

  Third arg KEYMAP is a keymap to use whilst reading;
    if omitted or nil, the default is `minibuffer-local-map'.

  If fourth arg READ is non-nil, interpret the result as a Lisp object
    and return that object:
    in other words, do `(car (read-from-string INPUT-STRING))'

  Fifth arg HIST, if non-nil, specifies a history list and optionally
    the initial position in the list.  It can be a symbol, which is the
    history list variable to use, or a cons cell (HISTVAR . HISTPOS).
    In that case, HISTVAR is the history list variable to use, and
    HISTPOS is the initial position for use by the minibuffer history
    commands.  For consistency, you should also specify that element of
    the history as the value of INITIAL-CONTENTS.  Positions are counted
    starting from 1 at the beginning of the list.

  Sixth arg DEFAULT-VALUE, if non-nil, should be a string, which is used
    as the default to `read' if READ is non-nil and the user enters
    empty input.  But if READ is nil, this function does _not_ return
    DEFAULT-VALUE for empty input!  Instead, it returns the empty string.

    Whatever the value of READ, DEFAULT-VALUE is made available via the
    minibuffer history commands.  DEFAULT-VALUE can also be a list of
    strings, in which case all the strings are available in the history,
    and the first string is the default to `read' if READ is non-nil.

  Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
   the current input method and the setting of `enable-multibyte-characters'.

  If the variable `minibuffer-allow-text-properties' is non-nil,
   then the string which is returned includes whatever text properties
   were present in the minibuffer.  Otherwise the value has no text properties.

  The remainder of this documentation string describes the
  INITIAL-CONTENTS argument in more detail.  It is only relevant when
  studying existing code, or when HIST is a cons.  If non-nil,
  INITIAL-CONTENTS is a string to be inserted into the minibuffer before
  reading input.  Normally, point is put at the end of that string.
  However, if INITIAL-CONTENTS is (STRING . POSITION), the initial
  input is STRING, but point is placed at _one-indexed_ position
  POSITION in the minibuffer.  Any integer value less than or equal to
  one puts point at the beginning of the string.  *Note* that this
  behavior differs from the way such arguments are used in `completing-read'
  and some related functions, which use zero-indexing for POSITION."
  )

(defun assoc-string (key list &optional case-fold)
  "Like `assoc' but specifically for strings (and symbols).

  This returns the first element of LIST whose car matches the string or
  symbol KEY, or nil if no match exists.  When performing the
  comparison, symbols are first converted to strings, and unibyte
  strings to multibyte.  If the optional arg CASE-FOLD is non-nil, case
  is ignored.

  Unlike `assoc', KEY can also match an entry in LIST consisting of a
  single string, rather than a cons cell whose car is a string."
  (some #(and (if (satisfies? ICons %)
                (fns/equal key (str (car %)))
                (fns/equal key %)) %) (seq list)))

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

(defn ^:private filter-completions [collection predicate]
  (map #(if (satisfies? ICons %) (car %) %)
       (filter (if predicate (el/fun predicate) identity) collection)))

(defn ^:private try-completion-internal [string collection]
  (when-let [completions (seq (filter #(.startsWith (str %) string) collection))]
    (reduce #(loop [n (count %1)]
               (let [prefix (subs %1 0 n)]
                 (if (.startsWith %2 prefix) prefix (recur (dec n)))))
            (sort completions))))

(defun try-completion (string collection &optional predicate)
  "Return common substring of all completions of STRING in COLLECTION.
  Test each possible completion specified by COLLECTION
  to see if it begins with STRING.  The possible completions may be
  strings or symbols.  Symbols are converted to strings before testing,
  see `symbol-name'.
  All that match STRING are compared together; the longest initial sequence
  common to all these matches is the return value.
  If there is no match at all, the return value is nil.
  For a unique match which is exact, the return value is t.

  If COLLECTION is an alist, the keys (cars of elements) are the
  possible completions.  If an element is not a cons cell, then the
  element itself is the possible completion.
  If COLLECTION is a hash-table, all the keys that are strings or symbols
  are the possible completions.
  If COLLECTION is an obarray, the names of all symbols in the obarray
  are the possible completions.

  COLLECTION can also be a function to do the completion itself.
  It receives three arguments: the values STRING, PREDICATE and nil.
  Whatever it returns becomes the value of `try-completion'.

  If optional third argument PREDICATE is non-nil,
  it is used to test each possible match.
  The match is a candidate only if PREDICATE returns non-nil.
  The argument given to PREDICATE is the alist element
  or the symbol from the obarray.  If COLLECTION is a hash-table,
  predicate is called with two arguments: the key and the value.
  Additionally to this predicate, `completion-regexp-list'
  is used to further constrain the set of candidates."
  (let [collection (filter-completions collection predicate)]
    (when-let [completion (try-completion-internal string collection)]
      (if (= 1 (count (filter #{string} collection)))
        true
        completion))))

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
  `all-completions', otherwise invoke `test-completion'.

  The arguments STRING and PREDICATE are as in `try-completion',
  `all-completions', and `test-completion'."
  )

(defun all-completions (string collection &optional predicate)
  "Search for partial matches to STRING in COLLECTION.
  Test each of the possible completions specified by COLLECTION
  to see if it begins with STRING.  The possible completions may be
  strings or symbols.  Symbols are converted to strings before testing,
  see `symbol-name'.
  The value is a list of all the possible completions that match STRING.

  If COLLECTION is an alist, the keys (cars of elements) are the
  possible completions.  If an element is not a cons cell, then the
  element itself is the possible completion.
  If COLLECTION is a hash-table, all the keys that are strings or symbols
  are the possible completions.
  If COLLECTION is an obarray, the names of all symbols in the obarray
  are the possible completions.

  COLLECTION can also be a function to do the completion itself.
  It receives three arguments: the values STRING, PREDICATE and t.
  Whatever it returns becomes the value of `all-completions'.

  If optional third argument PREDICATE is non-nil,
  it is used to test each possible match.
  The match is a candidate only if PREDICATE returns non-nil.
  The argument given to PREDICATE is the alist element
  or the symbol from the obarray.  If COLLECTION is a hash-table,
  predicate is called with two arguments: the key and the value.
  Additionally to this predicate, `completion-regexp-list'
  is used to further constrain the set of candidates.

  An obsolete optional fourth argument HIDE-SPACES is still accepted for
  backward compatibility.  If non-nil, strings in COLLECTION that start
  with a space are ignored unless STRING itself starts with a space."
  (let [collection (filter-completions collection predicate)
        prefix (try-completion-internal string collection)]
    (apply alloc/list (filter #(.startsWith (str %) prefix) collection))))

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

(defun active-minibuffer-window ()
  "Return the currently active minibuffer window, or nil if none."
  )

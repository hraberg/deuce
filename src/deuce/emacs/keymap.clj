(ns
 deuce.emacs.keymap
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun make-sparse-keymap (&optional string)
  "Construct and return a new sparse keymap.
  Its car is `keymap' and its cdr is an alist of (CHAR . DEFINITION),
  which binds the character CHAR to DEFINITION, or (SYMBOL . DEFINITION),
  which binds the function key or mouse event SYMBOL to DEFINITION.
  Initially the alist is nil."
  )

(defun define-key (keymap key def)
  "In KEYMAP, define key sequence KEY as DEF.
  KEYMAP is a keymap."
  )

(defun copy-keymap (keymap)
  "Return a copy of the keymap KEYMAP.
  The copy starts out with the same definitions of KEYMAP,
  but changing either the copy or KEYMAP does not affect the other.
  Any key definitions that are subkeymaps are recursively copied.
  However, a key definition which is a symbol whose definition is a keymap
  is not copied."
  )

(defun map-keymap-internal (function keymap)
  "Call FUNCTION once for each event binding in KEYMAP.
  FUNCTION is called with two arguments: the event that is bound, and
  the definition it is bound to.  The event may be a character range.
  If KEYMAP has a parent, this function returns it without processing it."
  )

(defun current-local-map ()
  "Return current buffer's local keymap, or nil if it has none.
  Normally the local keymap is set by the major mode with `use-local-map'."
  )

(defun where-is-internal (definition &optional keymap firstonly noindirect no-remap)
  "Return list of keys that invoke DEFINITION.
  If KEYMAP is a keymap, search only KEYMAP and the global keymap.
  If KEYMAP is nil, search all the currently active keymaps.
  If KEYMAP is a list of keymaps, search only those keymaps."
  )

(defun keymapp (object)
  "Return t if OBJECT is a keymap."
  )

(defun text-char-description (character)
  "Return a pretty description of file-character CHARACTER.
  Control characters turn into \"^char\", etc.  This differs from
  `single-key-description' which turns them into \"C-char\".
  Also, this function recognizes the 2**7 bit as the Meta character,
  whereas `single-key-description' uses the 2**27 bit for Meta.
  See Info node `(elisp)Describing Characters' for examples."
  )

(defun current-active-maps (&optional olp position)
  "Return a list of the currently active keymaps.
  OLP if non-nil indicates that we should obey `overriding-local-map' and
  `overriding-terminal-local-map'.  POSITION can specify a click position
  like in the respective argument of `key-binding'."
  )

(defun key-binding (key &optional accept-default no-remap position)
  "Return the binding for command KEY in current keymaps.
  KEY is a string or vector, a sequence of keystrokes.
  The binding is probably a symbol with a function definition."
  )

(defun map-keymap (function keymap)
  "Call FUNCTION once for each event binding in KEYMAP.
  FUNCTION is called with two arguments: the event that is bound, and
  the definition it is bound to.  The event may be a character range."
  )

(defun keymap-prompt (map)
  "Return the prompt-string of a keymap MAP.
  If non-nil, the prompt is shown in the echo-area
  when reading a key-sequence to be looked-up in this keymap."
  )

(defun apropos-internal (regexp &optional predicate)
  "Show all symbols whose names contain match for REGEXP.
  If optional 2nd arg PREDICATE is non-nil, (funcall PREDICATE SYMBOL) is done
  for each symbol and a symbol is mentioned only if that returns non-nil.
  Return list of symbols found."
  )

(defun set-keymap-parent (keymap parent)
  "Modify KEYMAP to set its parent map to PARENT.
  Return PARENT.  PARENT should be nil or another keymap."
  )

(defun current-minor-mode-maps ()
  "Return a list of keymaps for the minor modes of the current buffer."
  )

(defun make-keymap (&optional string)
  "Construct and return a new keymap, of the form (keymap CHARTABLE . ALIST).
  CHARTABLE is a char-table that holds the bindings for all characters
  without modifiers.  All entries in it are initially nil, meaning
  \"command undefined\".  ALIST is an assoc-list which holds bindings for
  function keys, mouse events, and any other things that appear in the
  input stream.  Initially, ALIST is nil."
  )

(defun describe-buffer-bindings (buffer &optional prefix menus)
  "Insert the list of all defined keys and their definitions.
  The list is inserted in the current buffer, while the bindings are
  looked up in BUFFER.
  The optional argument PREFIX, if non-nil, should be a key sequence;
  then we display only bindings that start with that prefix.
  The optional argument MENUS, if non-nil, says to mention menu bindings.
  (Ordinarily these are omitted from the output.)"
  )

(defun accessible-keymaps (keymap &optional prefix)
  "Find all keymaps accessible via prefix characters from KEYMAP.
  Returns a list of elements of the form (KEYS . MAP), where the sequence
  KEYS starting from KEYMAP gets you to MAP.  These elements are ordered
  so that the KEYS increase in length.  The first element is ([] . KEYMAP).
  An optional argument PREFIX, if non-nil, should be a key sequence;
  then the value includes only maps for prefixes that start with PREFIX."
  )

(defun lookup-key (keymap key &optional accept-default)
  "In keymap KEYMAP, look up key sequence KEY.  Return the definition.
  A value of nil means undefined.  See doc of `define-key'
  for kinds of definitions."
  )

(defun key-description (keys &optional prefix)
  "Return a pretty description of key-sequence KEYS.
  Optional arg PREFIX is the sequence of keys leading up to KEYS.
  Control characters turn into \"C-foo\" sequences, meta into \"M-foo\",
  spaces are put between sequence elements, etc."
  )

(defun single-key-description (key &optional no-angles)
  "Return a pretty description of command character KEY.
  Control characters turn into C-whatever, etc.
  Optional argument NO-ANGLES non-nil means don't put angle brackets
  around function keys and event symbols."
  )

(defun use-local-map (keymap)
  "Select KEYMAP as the local keymap.
  If KEYMAP is nil, that means no local keymap."
  )

(defun local-key-binding (keys &optional accept-default)
  "Return the binding for command KEYS in current local keymap only.
  KEYS is a string or vector, a sequence of keystrokes.
  The binding is probably a symbol with a function definition."
  )

(defun define-prefix-command (command &optional mapvar name)
  "Define COMMAND as a prefix command.  COMMAND should be a symbol.
  A new sparse keymap is stored as COMMAND's function definition and its value.
  If a second optional argument MAPVAR is given, the map is stored as
  its value instead of as COMMAND's value; but COMMAND is still defined
  as a function.
  The third optional argument NAME, if given, supplies a menu name
  string for the map.  This is required to use the keymap as a menu.
  This function returns COMMAND."
  )

(defun keymap-parent (keymap)
  "Return the parent keymap of KEYMAP.
  If KEYMAP has no parent, return nil."
  )

(defun global-key-binding (keys &optional accept-default)
  "Return the binding for command KEYS in current global keymap only.
  KEYS is a string or vector, a sequence of keystrokes.
  The binding is probably a symbol with a function definition.
  This function's return values are the same as those of `lookup-key'
  (which see)."
  )

(defun current-global-map ()
  "Return the current global keymap."
  )

(defun command-remapping (command &optional position keymaps)
  "Return the remapping for command COMMAND.
  Returns nil if COMMAND is not remapped (or not a symbol)."
  )

(defun minor-mode-key-binding (key &optional accept-default)
  "Find the visible minor mode bindings of KEY.
  Return an alist of pairs (MODENAME . BINDING), where MODENAME is
  the symbol which names the minor mode binding KEY, and BINDING is
  KEY's definition in that mode.  In particular, if KEY has no
  minor-mode bindings, return nil.  If the first binding is a
  non-prefix, all subsequent bindings will be omitted, since they would
  be ignored.  Similarly, the list doesn't include non-prefix bindings
  that come after prefix bindings."
  )

(defun describe-vector (vector &optional describer)
  "Insert a description of contents of VECTOR.
  This is text showing the elements of vector matched against indices.
  DESCRIBER is the output function used; nil means use `princ'."
  )

(defun use-global-map (keymap)
  "Select KEYMAP as the global keymap."
  )

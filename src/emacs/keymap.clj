(ns emacs.keymap (use [deuce.core]) (:refer-clojure :only []))

(defun make-sparse-keymap (&optional string)
  "Construct and return a new sparse keymap.\nIts car is `keymap' and its cdr is an alist of (CHAR . DEFINITION),\nwhich binds the character CHAR to DEFINITION, or (SYMBOL . DEFINITION),\nwhich binds the function key or mouse event SYMBOL to DEFINITION.\nInitially the alist is nil."
  )

(defun define-key (keymap key def)
  "In KEYMAP, define key sequence KEY as DEF.\nKEYMAP is a keymap."
  )

(defun copy-keymap (keymap)
  "Return a copy of the keymap KEYMAP.\nThe copy starts out with the same definitions of KEYMAP,\nbut changing either the copy or KEYMAP does not affect the other.\nAny key definitions that are subkeymaps are recursively copied.\nHowever, a key definition which is a symbol whose definition is a keymap\n"
  )

(defun map-keymap-internal (function keymap)
  "Call FUNCTION once for each event binding in KEYMAP.\nFUNCTION is called with two arguments: the event that is bound, and\nthe definition it is bound to.  The event may be a character range.\nIf KEYMAP has a parent, this function returns it without processing it.scroll-other-window is an interactive built-in function in `C source\ncode'."
  )

(defun current-local-map ()
  "Return current buffer's local keymap, or nil if it has none.\n"
  )

(defun where-is-internal (definition &optional keymap firstonly noindirect no-remap)
  "Return list of keys that invoke DEFINITION.\nIf KEYMAP is a keymap, search only KEYMAP and the global keymap.\nIf KEYMAP is nil, search all the currently active keymaps.\nIf KEYMAP is a list of keymaps, search only those keymaps."
  )

(defun keymapp (object)
  "Return t if OBJECT is a keymap."
  )

(defun text-char-description (character)
  "Return a pretty description of file-character CHARACTER.\nControl characters turn into \"^char\", etc.  This differs from\n`single-key-description' which turns them into \"C-char\".\nAlso, this function recognizes the 2**7 bit as the Meta character,\nwhereas `single-key-description' uses the 2**27 bit for Meta.\nSee Info node `(elisp)Describing Characters' for examples.modify-syntax-entry is an interactive built-in function in `C source\ncode'."
  )

(defun current-active-maps (&optional olp position)
  "Return a list of the currently active keymaps.\nOLP if non-nil indicates that we should obey `overriding-local-map' and\n`overriding-terminal-local-map'.  POSITION can specify a click position\nlike in the respective argument of `key-binding'.word-search-backward is an interactive built-in function in `C source\ncode'."
  )

(defun key-binding (key &optional accept-default no-remap position)
  "Return the binding for command KEY in current keymaps.\nKEY is a string or vector, a sequence of keystrokes.\nThe binding is probably a symbol with a function definition."
  )

(defun map-keymap (function keymap)
  "Call FUNCTION once for each event binding in KEYMAP.\nFUNCTION is called with two arguments: the event that is bound, and\nthe definition it is bound to.  The event may be a character range."
  )

(defun keymap-prompt (map)
  "Return the prompt-string of a keymap MAP.\nIf non-nil, the prompt is shown in the echo-area\n"
  )

(defun apropos-internal (regexp &optional predicate)
  "Show all symbols whose names contain match for REGEXP.\nIf optional 2nd arg PREDICATE is non-nil, (funcall PREDICATE SYMBOL) is done\nfor each symbol and a symbol is mentioned only if that returns non-nil.\n"
  )

(defun set-keymap-parent (keymap parent)
  "Modify KEYMAP to set its parent map to PARENT.\n"
  )

(defun current-minor-mode-maps ()
  )

(defun make-keymap (&optional string)
  "Construct and return a new keymap, of the form (keymap CHARTABLE . ALIST).\nCHARTABLE is a char-table that holds the bindings for all characters\nwithout modifiers.  All entries in it are initially nil, meaning\n\"command undefined\".  ALIST is an assoc-list which holds bindings for\nfunction keys, mouse events, and any other things that appear in the\ninput stream.  Initially, ALIST is nil."
  )

(defun describe-buffer-bindings (buffer &optional prefix menus)
  "Insert the list of all defined keys and their definitions.\nThe list is inserted in the current buffer, while the bindings are\nlooked up in BUFFER.\nThe optional argument PREFIX, if non-nil, should be a key sequence;\nthen we display only bindings that start with that prefix.\nThe optional argument MENUS, if non-nil, says to mention menu bindings.\n"
  )

(defun accessible-keymaps (keymap &optional prefix)
  "Find all keymaps accessible via prefix characters from KEYMAP.\nReturns a list of elements of the form (KEYS . MAP), where the sequence\nKEYS starting from KEYMAP gets you to MAP.  These elements are ordered\nso that the KEYS increase in length.  The first element is ([] . KEYMAP).\nAn optional argument PREFIX, if non-nil, should be a key sequence;\n"
  )

(defun lookup-key (keymap key &optional accept-default)
  "In keymap KEYMAP, look up key sequence KEY.  Return the definition.\nA value of nil means undefined.  See doc of `define-key'\nfor kinds of definitions."
  )

(defun key-description (keys &optional prefix)
  "Return a pretty description of key-sequence KEYS.\nOptional arg PREFIX is the sequence of keys leading up to KEYS.\nControl characters turn into \"C-foo\" sequences, meta into \"M-foo\",\n"
  )

(defun single-key-description (key &optional no-angles)
  "Return a pretty description of command character KEY.\nControl characters turn into C-whatever, etc.\nOptional argument NO-ANGLES non-nil means don't put angle brackets\n"
  )

(defun use-local-map (keymap)
  "Select KEYMAP as the local keymap.\n"
  )

(defun local-key-binding (keys &optional accept-default)
  "Return the binding for command KEYS in current local keymap only.\nKEYS is a string or vector, a sequence of keystrokes.\nThe binding is probably a symbol with a function definition."
  )

(defun define-prefix-command (command &optional mapvar name)
  "Define COMMAND as a prefix command.  COMMAND should be a symbol.\nA new sparse keymap is stored as COMMAND's function definition and its value.\nIf a second optional argument MAPVAR is given, the map is stored as\nits value instead of as COMMAND's value; but COMMAND is still defined\nas a function.\nThe third optional argument NAME, if given, supplies a menu name\nstring for the map.  This is required to use the keymap as a menu.\n"
  )

(defun keymap-parent (keymap)
  "Return the parent keymap of KEYMAP.\nIf KEYMAP has no parent, return nil.upcase-initials-region is an interactive built-in function in `C\nsource code'."
  )

(defun global-key-binding (keys &optional accept-default)
  "Return the binding for command KEYS in current global keymap only.\nKEYS is a string or vector, a sequence of keystrokes.\nThe binding is probably a symbol with a function definition.\nThis function's return values are the same as those of `lookup-key'\n(which see)."
  )

(defun current-global-map ()
  )

(defun command-remapping (command &optional position keymaps)
  "Return the remapping for command COMMAND.\nReturns nil if COMMAND is not remapped (or not a symbol)."
  )

(defun minor-mode-key-binding (key &optional accept-default)
  "Find the visible minor mode bindings of KEY.\nReturn an alist of pairs (MODENAME . BINDING), where MODENAME is\nthe symbol which names the minor mode binding KEY, and BINDING is\nKEY's definition in that mode.  In particular, if KEY has no\nminor-mode bindings, return nil.  If the first binding is a\nnon-prefix, all subsequent bindings will be omitted, since they would\nbe ignored.  Similarly, the list doesn't include non-prefix bindings\nthat come after prefix bindings."
  )

(defun describe-vector (vector &optional describer)
  "Insert a description of contents of VECTOR.\nThis is text showing the elements of vector matched against indices.\n"
  )

(defun use-global-map (keymap)
  )

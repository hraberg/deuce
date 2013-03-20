(ns deuce.emacs.data
  (:use [deuce.emacs-lisp :only (defun defvar setq setq-default) :as el])
  (:require [clojure.core :as c]
            [deuce.emacs-lisp :as el]
            [deuce.emacs-lisp.globals :as globals]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs-lisp.cons :as cons]
            [taoensso.timbre :as timbre])
  (:import [java.nio ByteOrder]
           [java.io Writer]
           [clojure.lang Symbol Var])
  (:refer-clojure :exclude [+ * - / aset set < = > max >= <= mod atom min]))

(declare consp car cdr set-default default-boundp markerp)

(defvar most-positive-fixnum Long/MAX_VALUE
  "The largest value that is representable in a Lisp integer.")

(defvar most-negative-fixnum Long/MIN_VALUE
  "The smallest value that is representable in a Lisp integer.")

(def ^:private array-class (Class/forName "[Ljava.lang.Object;"))

(defmethod print-method array-class [o ^Writer w]
  (print-method (vec (cons/ellipsis o)) w))

(defmethod print-dup array-class [array ^Writer w]
  (.write w "#el/vec [")
  (dotimes [idx (count array)]
    (print-dup (aget array idx) w)
    (when-not (c/= idx (dec (count array)))
      (.write w " ")))
  (.write w "]"))

(defmethod print-dup Symbol [s ^Writer w]
  (if (and (re-find  #"([\\,/]|^\d|::)" (name s)) (not= "/" (name s)))
    (.write w (str "#el/sym " (pr-str (name s))))
    (.write w (str s))))

(defrecord CharTable
    [;; /* This holds a default value,
     ;; which is used whenever the value for a specific character is nil.  */
     defalt

     ;; /* This points to another char table, which we inherit from when the
     ;; value for a specific character is nil.  The `defalt' slot takes
     ;; precedence over this.  */
     parent

     ;; /* This is a symbol which says what kind of use this char-table is
     ;; meant for.  */
     purpose

     contents ;[(1 << CHARTAB_SIZE_BITS_0)]

     ;; /* These hold additional data.  It is a vector.  */
     extras])

(defmethod print-method CharTable [char-table w]
  (.write w (str "#^" (vec (cons/ellipsis (concat [(.defalt char-table)
                                                   @(.parent char-table)
                                                   (.purpose char-table)]
                                                  (.contents char-table)))))))

;; struct buffer_text in buffer.h. Contains many other low level fields we hopefully won't need.
(defrecord BufferText
    [;; /* Actual address of buffer contents.  If REL_ALLOC is defined,
     ;;    this address might change when blocks are relocated which can
     ;;    e.g. happen when malloc is called.  So, don't pass a pointer
     ;;    into a buffer's text to functions that malloc.  */
     beg;

     ;; /* This counts buffer-modification events
     ;;    for this buffer.  It is incremented for
     ;;    each such event, and never otherwise
     ;;    changed.  */
     modiff

     ;;	/* Previous value of modiff, as of last
     ;;    time buffer visited or saved a file.  */
     save-modiff

     ;; /* The markers that refer to this buffer.
     ;;    This is actually a single marker ---
     ;;    successive elements in its marker `chain'
     ;;    are the other markers referring to this buffer.
     ;;    This is a singly linked unordered list, which means that it's
     ;;    very cheap to add a marker to the list and it's also very cheap
     ;;    to move a marker within a buffer.  */
     markers])

;; struct buffer in buffer.h. Pretty large, so won't move it all over at once.
;; The buffer locals are specified with DEFVAR_PER_BUFFER, and their defaults with DEFVAR_BUFFER_DEFAULTS
;; Thet're all already defined as globals in buffer.clj. We need to put them (with default value at create) on this guy somehow.
(defrecord Buffer
    [;; /* This structure holds the coordinates of the buffer contents
     ;;    in ordinary buffers.  In indirect buffers, this is not used.  */
     own-text

     ;; /* This points to the `struct buffer_text' that used for this buffer.
     ;;    In an ordinary buffer, this is the own_text field above.
     ;;    In an indirect buffer, this is the own_text field of another buffer.  */
     text

     ;; /* Char position of point in buffer.  */
     pt

     ;; /* The name of this buffer.  */
     name

     ;; /* "The mark".  This is a marker which may
     ;;    point into this buffer or may point nowhere.  */
     mark

     ;; /* Alist of elements (SYMBOL . VALUE-IN-THIS-BUFFER) for all
     ;;    per-buffer variables of this buffer.  For locally unbound
     ;;    symbols, just the symbol appears as the element.  */
     local-var-alist])

(defmethod print-method Buffer [buffer w]
  (.write w (str "#<buffer " @(.name buffer) ">")))

;; struct in buffer.h. Has many other fields, its also a linked list above. Attempting to use as a value object
(defrecord Marker
    [;; /* 1 means normal insertion at the marker's position
      ;;   leaves the marker after the inserted text.  */
     insertion-type

     ;; /* This is the buffer that the marker points into, or 0 if it points nowhere.
     ;;    Note: a chain of markers can contain markers pointing into different
     ;;    buffers (the chain is per buffer_text rather than per buffer, so it's
     ;;    shared between indirect buffers).  */
     ;; /* This is used for (other than NULL-checking):
     ;;    - Fmarker_buffer
     ;;    - Fset_marker: check eq(oldbuf, newbuf) to avoid unchain+rechain.
     ;;    - unchain_marker: to find the list from which to unchain.
     ;;    - Fkill_buffer: to only unchain the markers of current indirect buffer.
     ;;    */
     buffer;

     ;; /* This is the char position where the marker points.  */
     charpos])

(defmethod print-method Marker [marker w]
  (.write w (str "#<marker" (if @(.buffer marker)
                              (str " at " @(.charpos marker) " in " @(.name @(.buffer marker)))
                              (str " in no buffer")) ">")))

(defrecord Frame
    [;; /* Name of this frame: a Lisp string.  It is used for looking up resources,
     ;;    as well as for the title in some cases.  */
     name

     ;; /* This frame's root window.  Every frame has one.
     ;;    If the frame has only a minibuffer window, this is it.
     ;;    Otherwise, if the frame has a minibuffer window, this is its sibling.  */
     root-window

     ;; /* This frame's selected window.
     ;;    Each frame has its own window hierarchy
     ;;    and one of the windows in it is selected within the frame.
     ;;    The selected window of the selected frame is Emacs's selected window.  */
     selected-window

     ;; /* This frame's minibuffer window.
     ;;    Most frames have their own minibuffer windows,
     ;;    but only the selected frame's minibuffer window
     ;;    can actually appear to exist.  */
     minibuffer-window

     ;; /* The terminal device that this frame uses.  If this is NULL, then
     ;;    the frame has been deleted. */
     terminal])

(defmethod print-method Frame [frame w]
  (.write w (str "#<frame " (.name frame) " "
                 (format "0x%x" (System/identityHashCode frame)) ">")))

(defrecord Window
    [;; /* t if this window is a minibuffer window.  */
     mini-p

     ;; /* Following (to right or down) and preceding (to left or up) child
     ;;    at same level of tree.  */
     next prev

     ;; /* First child of this window: vchild is used if this is a vertical
     ;;    combination, hchild if this is a horizontal combination.  Of the
     ;;    fields vchild, hchild and buffer, one and only one is non-nil
     ;;    unless the window is dead.  */
     hchild
     vchild

     ;; /* The window this one is a child of.  */
     parent

     ;; /* The upper left corner coordinates of this window, as integers
     ;;    relative to upper left corner of frame = 0, 0.  */
     left-col
     top-line

     ;; /* The size of the window.  */
     total-lines
     total-cols

     ;; /* The normal size of the window.  */
     normal-lines
     normal-cols

     ;; /* The buffer displayed in this window.  Of the fields vchild,
     ;;    hchild and buffer, one and only one is non-nil unless the window
     ;;    is dead.  */
     buffer

     ;; /* A marker pointing to where in the text to start displaying.
     ;;    BIDI Note: This is the _logical-order_ start, i.e. the smallest
     ;;    buffer position visible in the window, not necessarily the
     ;;    character displayed in the top left corner of the window.  */
     start

     ;; /* A marker pointing to where in the text point is in this window,
     ;;    used only when the window is not selected.
     ;;    This exists so that when multiple windows show one buffer
     ;;    each one can have its own value of point.  */
     pointm

     ;; /* Unique number of window assigned when it was created.  */
     sequence-number])

(defmethod print-method Window [window w]
  (.write w (str "#<window " (.sequence-number window)
                 (when-let [buffer @(.buffer window)]
                   (str " on " @(.name buffer))) ">")))

(defn ^:private promote-char [x]
  (cond
   (char? x) (int x)
   (markerp x) @(.charpos x)
   :else x))

(defn ^:private promote-chars [xs]
  (map promote-char xs))

(defun natnump (object)
  "Return t if OBJECT is a nonnegative integer."
  ((every-pred integer? (complement neg?)) object))

(defun markerp (object)
  "Return t if OBJECT is a marker (editor pointer)."
  (instance? Marker object))

(defun ash (value count)
  "Return VALUE with its bits shifted left by COUNT.
  If COUNT is negative, shifting is actually to the right.
  In this case, the sign bit is duplicated."
  (if (pos? count)
    (bit-shift-left value count)
    (bit-shift-right value (c/- count))))

(defun type-of (object)
  "Return a symbol representing the type of OBJECT.
  The symbol returned names the object's basic type;
  for example, (type-of 1) returns `integer'."
  (type object))

(declare symbol-function symbolp eq)

;; Navgeet's additions, not sure they've been tested against real world scenarios.
(defn ^:private indirect_function [object]
  (loop [hare object
         tortoise object]
    (if (symbolp hare)
      (let [hare1 (symbol-function hare)]
        (if (symbolp hare1)
          (let [hare2 (symbol-function hare1)
                tortoise1 (symbol-function tortoise)]
            (if (eq hare2 tortoise1)
              (el/throw 'cyclic-function-indirection '(object))
              (recur hare2 tortoise1)))
          hare1))
      hare)))

(defun indirect-function (object &optional noerror)
  "Return the function at the end of OBJECT's function chain.
  If OBJECT is not a symbol, just return it.  Otherwise, follow all
  function indirections to find the final function binding and return it.
  If the final symbol in the chain is unbound, signal a void-function error.
  Optional arg NOERROR non-nil means to return nil instead of signaling.
  Signal a cyclic-function-indirection error if there is a loop in the
  function chain of symbols."
  (if (symbolp object)
    (el/try-with-tag
      (let [result (symbol-function object)]
        ;; Optimize for no indirection.
        (if (symbolp result)
          (indirect_function result)
          result))
      (catch 'void-function e
        (if noerror nil (throw e))))
    object))

(defun symbol-name (symbol)
  "Return SYMBOL's name, a string."
  (el/check-type 'symbolp symbol)
  (condp some [symbol]
    keyword? (str symbol)
    symbol? (name symbol)
    true? "true"
    nil? (pr-str nil)))

(defun makunbound (symbol)
  "Make SYMBOL's value be void.
  Return SYMBOL."
  (ns-unmap 'deuce.emacs-lisp.globals (el/sym symbol))
  symbol)

(defun interactive-form (cmd)
  "Return the interactive form of CMD or nil if none.
  If CMD is not a command, the return value is nil.
  Value, if non-nil, is a list (interactive SPEC)."
  )

(defun logior (&rest ints-or-markers)
  "Return bitwise-or of all the arguments.
  Arguments may be integers, or markers converted to integers."
  (apply bit-or (promote-chars ints-or-markers)))

(declare arrayp listp)

(defun sequencep (object)
  "Return t if OBJECT is a sequence (list or array)."
  ((some-fn sequential? arrayp listp) object))

(defun zerop (number)
  "Return t if NUMBER is zero."
  (zero? number))

(declare symbol-value)

(defun indirect-variable (object)
  "Return the variable at the end of OBJECT's variable chain.
  If OBJECT is a symbol, follow all variable indirections and return the final
  variable.  If OBJECT is not a symbol, just return it.
  Signal a cyclic-variable-indirection error if there is a loop in the
  variable chain of symbols."
  (symbol-value object))

(defun symbol-value (symbol)
  "Return SYMBOL's value.  Error if that is void."
  (el/check-type 'symbolp symbol)
  (el/el-var-get* symbol))

(defun keywordp (object)
  "Return t if OBJECT is a keyword.
  This means that it is a symbol with a print name beginning with `:'
  interned in the initial obarray."
  (keyword? object))

(defun #el/sym "1+" (number)
  "Return NUMBER plus one.  NUMBER may be a number or a marker.
  Markers are converted to integers."
  (inc (promote-char number)))

(defun subrp (object)
  "Return t if OBJECT is a built-in function."
  (and (fn? object)
       (-> object meta :ns)
       (not= (the-ns 'deuce.emacs) (-> object meta :ns))))

(defun symbol-plist (symbol)
  "Return SYMBOL's property list."
  (cons/maybe-seq (map #(cons/pair (key val) (val %)) (@el/symbol-plists 'symbol))))

(defun stringp (object)
  "Return t if OBJECT is a string."
  (string? object))

(defun integerp (object)
  "Return t if OBJECT is an integer."
  (integer? object))

(defun fboundp (symbol)
  "Return t if SYMBOL's function definition is not void."
  (when-let [v (and symbol (el/fun symbol))]
    (bound? v)))

(defun % (x y)
  "Return remainder of X divided by Y.
  Both must be integers or markers."
  (rem x y))

(defun + (&rest numbers-or-markers)
  "Return sum of any number of arguments, which are numbers or markers."
  (apply c/+ (promote-chars numbers-or-markers)))

(defun lsh (value count)
  "Return VALUE with its bits shifted left by COUNT.
  If COUNT is negative, shifting is actually to the right.
  In this case, zeros are shifted in on the left."
  (if (pos? count)
    (bit-shift-left value count)
    (bit-shift-right value (c/- count))))

(declare null)

(defun eq (obj1 obj2)
  "Return t if the two args are the same Lisp object."
  (cond
   (null obj1) (null obj2)
    ;; Macros can get confused by the exact namespace
   (and (symbol? obj1) (symbol? obj2)) (c/= (name obj1) (name obj2))
   (or (char? obj1) (char? obj2)) (c/= (promote-char obj1) (promote-char obj2))
   :else (identical? obj1 obj2)))

(defun * (&rest numbers-or-markers)
  "Return product of any number of arguments, which are numbers or markers."
  (apply c/* (promote-chars numbers-or-markers)))

(defun - (&optional number-or-marker &rest more-numbers-or-markers)
  "Negate number or subtract numbers or markers and return the result.
  With one arg, negates it.  With more than one arg,
  subtracts all but the first from the first."
  (apply c/- (promote-chars (c/cons number-or-marker more-numbers-or-markers))))

(defun multibyte-string-p (object)
  "Return t if OBJECT is a multibyte string."
  (string? object))

(defun logxor (&rest ints-or-markers)
  "Return bitwise-exclusive-or of all the arguments.
  Arguments may be integers, or markers converted to integers."
  (apply c/bit-xor (promote-chars ints-or-markers))  )

(defun floatp (object)
  "Return t if OBJECT is a floating point number."
  (float? object))

(defun number-or-marker-p (object)
  "Return t if OBJECT is a number or a marker."
  (number? object))

(defun cdr-safe (object)
  "Return the cdr of OBJECT if it is a cons cell, or else nil."
  (when (consp object)
    (cdr object)))

(defun / (dividend divisor &rest divisors)
  "Return first argument divided by all the remaining arguments.
  The arguments must be numbers or markers."
  (if (zero? divisor)
    (el/throw 'arith-error nil)
    (c/reduce / (c/let [r (c/apply clojure.core// (promote-chars [dividend divisor]))]
                  (if (ratio? r) (long r) r))
              (promote-chars divisors))))

(defun byteorder ()
  "Return the byteorder for the machine.
  Returns 66 (ASCII uppercase B) for big endian machines or 108 (ASCII
  lowercase l) for small endian machines."
  ({ByteOrder/BIG_ENDIAN "B"
    ByteOrder/LITTLE_ENDIAN "l"}
   (ByteOrder/nativeOrder)))

(defun subr-name (subr)
  "Return name of subroutine SUBR.
  SUBR must be a built-in function."
  (-> subr meta :name))

(defun make-local-variable (variable)
  "Make VARIABLE have a separate value in the current buffer.
  Other buffers will continue to share a common default value.
  (The buffer-local value of VARIABLE starts out as the same value
  VARIABLE previously had.  If VARIABLE was void, it remains void.)
  Return VARIABLE.

  If the variable is already arranged to become local when set,
  this function causes a local value to exist for this buffer,
  just as setting the variable would do.

  This function returns VARIABLE, and therefore
    (set (make-local-variable 'VARIABLE) VALUE-EXP)
  works.

  See also `make-variable-buffer-local'.

  Do not use `make-local-variable' to make a hook variable buffer-local.
  Instead, use `add-hook' and specify t for the LOCAL argument."
  (let [buffer-locals (.local-var-alist ((el/fun 'current-buffer)))]
    (when-not (contains? @buffer-locals variable)
      (swap! buffer-locals assoc variable (Var/create)))
    variable))

(defun numberp (object)
  "Return t if OBJECT is a number (floating point or integer)."
  ((some-fn number? char?) object))

(defun logand (&rest ints-or-markers)
  "Return bitwise-and of all the arguments.
  Arguments may be integers, or markers converted to integers."
  (apply c/bit-and (promote-chars ints-or-markers)))

(defun consp (object)
  "Return t if OBJECT is a cons cell."
  (cons/consp object))

(defun listp (object)
  "Return t if OBJECT is a list, that is, a cons cell or nil.
  Otherwise, return nil."
  (cons/listp object))

(defun aref (array idx)
  "Return the element of ARRAY at index IDX.
  ARRAY may be a vector, a string, a char-table, a bool-vector,
  or a byte-code object.  IDX starts at 0."
  (if (instance? CharTable array)
    (aref (.contents ^CharTable array) idx)
    (get array idx)))

(defun wholenump (object)
  "Return t if OBJECT is a nonnegative integer."
  ((every-pred pos? integer?) object))

;; We probably don't want to do this due to interned strings.
(def ^:private string-chars (doto (.getDeclaredField String "value")
                                     (.setAccessible true)))

(defun aset (array idx newelt)
  "Store into the element of ARRAY at index IDX the value NEWELT.
  Return NEWELT.  ARRAY may be a vector, a string, a char-table or a
  bool-vector.  IDX starts at 0."
  (if (instance? CharTable array)
    (aset (.contents ^CharTable array) idx newelt)
    (if (string? array)
      (do (timbre/warn "modifying String" (str "\"" array "\""))
        (c/aset (.get string-chars array) idx (char newelt)))
      (c/aset array idx newelt))))

(declare vectorp)

(defun arrayp (object)
  "Return t if OBJECT is an array (string or vector)."
  ((some-fn vectorp stringp) object))

(defun vectorp (object)
  "Return t if OBJECT is a vector."
  (c/= array-class (type object)))

(defun fmakunbound (symbol)
  "Make SYMBOL's function definition be void.
  Return SYMBOL."
  (when-let [fun (el/fun (el/sym symbol))]
    (ns-unmap (-> (el/fun symbol) meta :ns) (el/sym symbol)))
  symbol)

(defun lognot (number)
  "Return the bitwise complement of NUMBER.  NUMBER must be an integer."
  (bit-not (promote-char number)))

(declare atom)

(defun setcdr (cell newcdr)
  "Set the cdr of CELL to be NEWCDR.  Returns NEWCDR."
  (el/check-type 'consp cell)
  (cons/setcdr cell (cons/maybe-seq newcdr)))

(defun set (symbol newval)
  "Set SYMBOL's value to NEWVAL, and return NEWVAL."
  (el/check-type 'symbolp symbol)
  (if (c/= symbol (c/symbol ""))
    nil ;; Hack for anonymous symbol used by abbrev.el, needs proper fix.
    ((eval `(fn set [v#] (setq ~symbol v#))) newval)))

(defun < (num1 num2)
  "Return t if first arg is less than second arg.  Both must be numbers or markers."
  (c/< (promote-char num1) (promote-char num2)))

(defun car-safe (object)
  "Return the car of OBJECT if it is a cons cell, or else nil."
  (when (consp object)
    (car object)))

(defun fset (symbol definition)
  "Set SYMBOL's function definition to DEFINITION, and return DEFINITION."
  (el/check-type 'symbolp symbol)
  (let [symbol (el/sym symbol)
        sym (el/fun symbol)]
    ;; Navgeet's additions, not sure they've been tested against real world scenarios.
    (when sym
      (when (and (consp @sym) (c/= (car @sym) 'autoload))
        ;; Creates a cyclic load dependency chain.
        ;; TODO: deuce.emacs.fns/put must be below deuce.emacs.data
        ;; (fns/put symbol 'autoload (cdr @sym))
        ))
    (ns-unmap 'deuce.emacs symbol)
    (intern 'deuce.emacs symbol definition)
    definition))

(defun cdr (list)
  "Return the cdr of LIST.  If arg is nil, return nil.
  Error if arg is not nil and not a cons cell.  See also `cdr-safe'.

  See Info node `(elisp)Cons Cells' for a discussion of related basic
  Lisp concepts such as cdr, car, cons cell and list."
  (el/check-type 'listp list)
  (cons/cdr (cons/maybe-seq list)))

(defun = (num1 num2)
  "Return t if two args, both numbers or markers, are equal."
  (== (promote-char num1) (promote-char num2)))

(defun make-variable-buffer-local (variable)
  "Make VARIABLE become buffer-local whenever it is set.
  At any time, the value for the current buffer is in effect,
  unless the variable has never been set in this buffer,
  in which case the default value is in effect.
  Note that binding the variable with `let', or setting it while
  a `let'-style binding made in this buffer is in effect,
  does not make the variable buffer-local.  Return VARIABLE.

  In most cases it is better to use `make-local-variable',
  which makes a variable local in just one buffer.

  The function `default-value' gets the default value and `set-default' sets it."
  (when-not (default-boundp variable)
    (set-default variable nil))
  (swap! el/buffer-locals conj variable)
  variable)

(defun char-or-string-p (object)
  "Return t if OBJECT is a character or a string."
  ((some-fn char? integer? string?) object))

(declare char-table-p)

(defun vector-or-char-table-p (object)
  "Return t if OBJECT is a char-table or vector."
  ((some-fn vectorp char-table-p) object))

(defun bufferp (object)
  "Return t if OBJECT is an editor buffer."
  (instance? Buffer object))

(defun > (num1 num2)
  "Return t if first arg is greater than second arg.  Both must be numbers or markers."
  (println num2 num2)
  (c/> (promote-char num1) (promote-char num2)))

(defun max (number-or-marker &rest numbers-or-markers)
  "Return largest of all the arguments (which must be numbers or markers).
  The value is always a number; markers are converted to numbers."
  (apply c/max number-or-marker numbers-or-markers))

(defun local-variable-if-set-p (variable &optional buffer)
  "Non-nil if VARIABLE will be local in buffer BUFFER when set there.
  More precisely, this means that setting the variable (with `set' or`setq'),
  while it does not have a `let'-style binding that was made in BUFFER,
  will produce a buffer local binding.  See Info node
  `(elisp)Creating Buffer-Local'.
  BUFFER defaults to the current buffer."
  )

(defun default-boundp (symbol)
  "Return t if SYMBOL has a non-void default value.
  This is the value that is seen in buffers that do not have their own values
  for this variable."
  (or (nil? symbol)
      (when-let [v (el/global symbol)]
        (.hasRoot v))))

(defun nlistp (object)
  "Return t if OBJECT is not a list.  Lists include nil."
  ((complement listp) object))

(defun >= (num1 num2)
  "Return t if first arg is greater than or equal to second arg.
  Both must be numbers or markers."
  (c/>= (promote-char num1) (promote-char num2)))

(defun boundp (symbol)
  "Return t if SYMBOL's value is not void."
  (or (nil? symbol)
      (when-let [v (el/global symbol)]
        (bound? v))))

(defun default-value (symbol)
  "Return SYMBOL's default value.
  This is the value that is seen in buffers that do not have their own values
  for this variable.  The default value is meaningful for variables with
  local bindings in certain buffers."
  (if-let [v (el/global symbol)]
    (.getRawRoot v)
    (el/throw* 'void-variable (list symbol))))

(defun setcar (cell newcar)
  "Set the car of CELL to be NEWCAR.  Returns NEWCAR."
  (el/check-type 'consp cell)
  (cons/setcar cell (cons/maybe-seq newcar)))

(defun symbolp (object)
  "Return t if OBJECT is a symbol."
  (or ((some-fn symbol? keyword? nil? true?) object)
      (and (seq? object) (c/= `deref (first object)) (symbolp (second object)))))

(defun <= (num1 num2)
  "Return t if first arg is less than or equal to second arg.
  Both must be numbers or markers."
  (c/<= (promote-char num1) (promote-char num2)))

(defun local-variable-p (variable &optional buffer)
  "Non-nil if VARIABLE has a local binding in buffer BUFFER.
  BUFFER defaults to the current buffer."
  (contains? (get-thread-bindings) variable))

(defun byte-code-function-p (object)
  "Return t if OBJECT is a byte-compiled function object."
  (fn? object))

(defun defalias (symbol definition &optional docstring)
  "Set SYMBOL's function definition to DEFINITION, and return DEFINITION.
  Associates the function with the current load file, if any.
  The optional third argument DOCSTRING specifies the documentation string
  for SYMBOL; if it is omitted or nil, SYMBOL uses the documentation string
  determined by DEFINITION."
  (let [lambda? (fn? definition)
        f (and (or lambda? (symbol? definition))  ;; guard against value
               (el/fun definition))]
    (when-let [alias (if-not lambda?
                       (fn alias [&form &env & args] ;; Note implicit macro args
                         `(el/progn (~(el/fun definition) ~@args)))
                       definition)]
      (ns-unmap 'deuce.emacs symbol)
      (el/defvar-helper* 'deuce.emacs symbol alias (str symbol " is an alias for `" definition "'.\n\n"
                                                        (or docstring
                                                            (-> f meta :doc)
                                                            (-> definition meta :doc))))
      (alter-meta! (el/fun symbol) assoc :alias definition)
      (when-not lambda? (.setMacro (ns-resolve 'deuce.emacs symbol)))
      ;; We want this, but it currently wrecks havoc due to backquote
      ;; (when (and lambda? (-> definition meta :macro)))
      ;;   (.setMacro (el/fun symbol))))
      )
    definition))

(defun setplist (symbol newplist)
  "Set SYMBOL's property list to NEWPLIST, and return NEWPLIST."
  (swap! el/symbol-plists assoc symbol (into {} (map #(vector (car %) (cdr %)) newplist)))
  newplist)

(defun set-default (symbol value)
  "Set SYMBOL's default value to VALUE.  SYMBOL and VALUE are evaluated.
  The default value is seen in buffers that do not have their own values
  for this variable."
  (el/check-type 'symbolp symbol)
  ((eval `(fn set-default [v#] (setq-default ~symbol v#))) value))

(defun symbol-function (symbol)
  "Return SYMBOL's function definition.  Error if that is void."
  (el/check-type 'symbolp symbol)
  (if-let [f (el/fun symbol)]
    (if (var? f) (el/fun f) f) ;; See binding/mode-specific-command-prefix
    (el/throw 'void-function symbol)))

(defun kill-local-variable (variable)
  "Make VARIABLE no longer have a separate value in the current buffer.
  From now on the default value will apply in this buffer.  Return VARIABLE."
  (let [buffer-locals (.local-var-alist ((el/fun 'current-buffer)))]
    (swap! buffer-locals dissoc variable)
    variable))

(defun car (list)
  "Return the car of LIST.  If arg is nil, return nil.
  Error if arg is not nil and not a cons cell.  See also `car-safe'.

  See Info node `(elisp)Cons Cells' for a discussion of related basic
  Lisp concepts such as car, cdr, cons cell and list."
  (el/check-type 'listp list)
  (cons/maybe-seq (cons/car list)))

(defun bool-vector-p (object)
  "Return t if OBJECT is a bool-vector."
  )

(defun subr-arity (subr)
  "Return minimum and maximum number of args allowed for SUBR.
  SUBR must be a built-in function.
  The returned value is a pair (MIN . MAX).  MIN is the minimum number
  of args.  MAX is the maximum number or the symbol `many', for a
  function with `&rest' args, or `unevalled' for a special form."
  (-> subr meta :arglists first count))

(defun mod (x y)
  "Return X modulo Y.
  The result falls between zero (inclusive) and Y (exclusive).
  Both X and Y must be numbers or markers."
  (c/mod x y))

(defun #el/sym "1-" (number)
  "Return NUMBER minus one.  NUMBER may be a number or a marker.
  Markers are converted to integers."
  (dec (promote-char number)))

(defun atom (object)
  "Return t if OBJECT is not a cons cell.  This includes nil."
  (not (consp object)))

(defun null (object)
  "Return t if OBJECT is nil."
  (when (or (nil? object) (c/= () object) (false? object))
    true))

(defun char-table-p (object)
  "Return t if OBJECT is a char-table."
  (instance? CharTable object))

(defun make-variable-frame-local (variable)
  "This function is obsolete since 22.2;
  explicitly check for a frame-parameter instead.

  Enable VARIABLE to have frame-local bindings.
  This does not create any frame-local bindings for VARIABLE,
  it just makes them possible.

  A frame-local binding is actually a frame parameter value.
  If a frame F has a value for the frame parameter named VARIABLE,
  that also acts as a frame-local binding for VARIABLE in F--
  provided this function has been called to enable VARIABLE
  to have frame-local bindings at all.

  The only way to create a frame-local binding for VARIABLE in a frame
  is to set the VARIABLE frame parameter of that frame.  See
  `modify-frame-parameters' for how to set frame parameters.

  Note that since Emacs 23.1, variables cannot be both buffer-local and
  frame-local any more (buffer-local bindings used to take precedence over
  frame-local bindings)."
  )

(defun number-to-string (number)
  "Return the decimal representation of NUMBER as a string.
  Uses a minus sign if negative.
  NUMBER may be an integer or a floating point number."
  (str number))

(defun integer-or-marker-p (object)
  "Return t if OBJECT is an integer or a marker (editor pointer)."
  ((some-fn integerp markerp) object))

(defun min (number-or-marker &rest numbers-or-markers)
  "Return smallest of all the arguments (which must be numbers or markers).
  The value is always a number; markers are converted to numbers."
  (apply c/min number-or-marker numbers-or-markers))

(defun string-to-number (string &optional base)
  "Parse STRING as a decimal number and return the number.
  This parses both integers and floating point numbers.
  It ignores leading spaces and tabs, and all trailing chars.

  If BASE, interpret STRING as a number in that base.  If BASE isn't
  present, base 10 is used.  BASE must be between 2 and 16 (inclusive).
  If the base used is not 10, STRING is always parsed as integer."
  (try
    (long (BigInteger. string (or base 10)))
    (catch NumberFormatException _
      (Double/parseDouble string))))

(defun variable-binding-locus (variable)
  "Return a value indicating where VARIABLE's current binding comes from.
  If the current binding is buffer-local, the value is the current buffer.
  If the current binding is frame-local, the value is the selected frame.
  If the current binding is global (the default), the value is nil."
  )

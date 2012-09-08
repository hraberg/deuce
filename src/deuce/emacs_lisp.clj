(ns deuce.emacs-lisp
  (require [clojure.core :as c]
           [clojure.walk :as w])
  (:refer-clojure :exclude [defmacro and or cond let while eval set])
  (import [deuce EmacsLispError]))

(deftype DottedPair [car cdr]
  Object
  (toString [this]
    (str "(" car ((fn tail [c]
                    (if (instance? DottedPair c)
                      (str " " (.car c) (tail (.cdr c)))
                      (when (c/and c (not= () c)) (str " . " c)))) cdr) ")")))

(defmethod print-method DottedPair [pair writer]
  (.write writer (str pair)))

(create-ns 'deuce.emacs)
(create-ns 'deuce.emacs-lisp.globals)

(defn ^:private clojure-special-forms []
  (->> (ns-map 'deuce.emacs-lisp)
       (filter (comp :clojure-special-form meta val))
       (into {})))

(defn ^:private cleanup-clojure [x]
  (if (symbol? x) (symbol (name x)) x))

(defn ^:private qualify-globals [locals form]
  (if-let [s (c/and (symbol? form)
                    (not (locals form))
                    (not (re-find #"\." (c/name form)))
                    (c/or
                     ((clojure-special-forms) (symbol (name form)))
                     (ns-resolve 'deuce.emacs-lisp.globals (symbol (name form)))))]
    (symbol (-> s meta :ns str) (-> s meta :name str))
    form))

(defn ^:private qualify-fns [form]
  (if-let [s (c/and (list? form) (symbol? (first form))
                    (ns-resolve 'deuce.emacs (symbol (name (first form)))))]
    (apply list (cons (symbol (-> s meta :ns str) (-> s meta :name str)) (next form)))
    (if (c/and (symbol? form) (= "deuce.emacs" (namespace form)))
      (symbol (name form))
      form)))

(defn ^:private strip-comments [form]
  (if (seq? form)
    (apply list (remove (every-pred seq? (comp '#{clojure.core/comment} first)) form))
    form))

(defn ^:private compile-body [args body]
  (try
    (c/eval `(fn ~(vec args)
               ~(->> body
                     (w/postwalk #(if (c/and (seq? %) (symbol? (first %))
                                             ('#{defun defmacro} (symbol (name (first %)))))
                                    ^:protect-from-expansion (fn [] %)
                                    %))
                     (w/postwalk strip-comments)
                     (w/postwalk (partial qualify-globals (c/set args)))
                     (w/postwalk qualify-fns)
                     (w/postwalk #(if (-> % meta :protect-from-expansion) (%) %)))))
    (catch RuntimeException e
      (println e)
      (println args)
      (println body (type body))
      (throw e))))

;; defined as fn in eval.clj
(c/defmacro eval
  "Evaluate FORM and return its value.
  If LEXICAL is t, evaluate using lexical scoping."
  {:arglists '([FORM &optional LEXICAL])}
  [body & [lexical]]
  (c/let [vars (keys &env)]
    `(binding [*ns* (the-ns 'deuce.emacs)]
       (~cleanup-clojure ((~compile-body '~vars ~body) ~@vars)))))

(declare let-helper*)

(c/defmacro def-helper* [what name arglist & body]
  (c/let [[docstring body] (split-with string? body)
          name (if (seq? name) (c/eval name) name)
          [arg & args :as arglist] (replace '{&rest &} arglist)
          [arglist &optional optional-args] (if (= '&optional arg)
                                              [() arg args]
                                              (partition-by '#{&optional} arglist))
          arglist (concat arglist (when &optional ['& (vec optional-args)]))
          [[interactive] body] (split-with #(c/and (seq? %)
                                                   (= 'interactive (first %))) body)
          emacs-lisp? (= (the-ns 'deuce.emacs) *ns*)
          doc (apply str docstring)
          the-args (remove '#{&} (flatten arglist))]
;    (println (c/name what) name (c/or (-> name meta :line) ""))
    `(c/let [f# (~what ~name ~(vec arglist)
                       ~(if emacs-lisp?
                          `(let-helper* false ~(map #(list % %) the-args) (eval '(do ~@body)))
                          `(do ~@body)))]
       (if (var? f#)
         (do
           (alter-meta! f# merge {:doc ~doc :line (-> '~name meta :line)
                                  :file (when-let [file# (ns-resolve 'deuce.emacs-lisp.globals 'load-file-name)]
                                          @file#)})
           (alter-var-root f# (constantly (with-meta @f# (meta f#)))))
         (with-meta f# (assoc (meta f#) :doc ~doc))))))

(c/defmacro defun
  "Define NAME as a function.
  The definition is (lambda ARGLIST [DOCSTRING] BODY...).
  See also the function `interactive'."
  {:arglists '([NAME ARGLIST [DOCSTRING] BODY...])}
  [name arglist & body]
  `(do (def-helper* defn ~name ~arglist ~@body)
       '~name))

;; defined in subr.el
(c/defmacro lambda
  "Return a lambda expression.
  A call of the form (lambda ARGS DOCSTRING INTERACTIVE BODY) is
  self-quoting; the result of evaluating the lambda expression is the
  expression itself.  The lambda expression may then be treated as a
  function, i.e., stored as the function value of a symbol, passed to
  `funcall' or `mapcar', etc.

  ARGS should take the same form as an argument list for a `defun'.
  DOCSTRING is an optional documentation string.
   If present, it should describe how to call the function.
   But documentation strings are usually not useful in nameless functions.
  INTERACTIVE should be a call to the function `interactive', which see.
  It may also be omitted.
  BODY should be a list of Lisp expressions."
  {:arglists '([ARGS [DOCSTRING] [INTERACTIVE] BODY])}
  [& cdr]
  `(def-helper* fn '~'lambda ~(first cdr) ~@(rest cdr)))

(c/defmacro unwind-protect
  "Do BODYFORM, protecting with UNWINDFORMS.
  If BODYFORM completes normally, its value is returned
  after executing the UNWINDFORMS.
  If BODYFORM exits nonlocally, the UNWINDFORMS are executed anyway."
  {:arglists '([BODYFORM UNWINDFORMS...])}
  [bodyform & unwindforms]
  (try
    ~bodyform
    (finally ~@unwindforms)))

(c/defmacro condition-case
  "Regain control when an error is signaled.
  Executes BODYFORM and returns its value if no error happens.
  Each element of HANDLERS looks like (CONDITION-NAME BODY...)
  where the BODY is made of Lisp expressions.

  A handler is applicable to an error
  if CONDITION-NAME is one of the error's condition names.
  If an error happens, the first applicable handler is run.

  The car of a handler may be a list of condition names instead of a
  single condition name; then it handles all of them.  If the special
  condition name `debug' is present in this list, it allows another
  condition in the list to run the debugger if `debug-on-error' and the
  other usual mechanisms says it should (otherwise, `condition-case'
  suppresses the debugger).

  When a handler handles an error, control returns to the `condition-case'
  and it executes the handler's BODY...
  with VAR bound to (ERROR-SYMBOL . SIGNAL-DATA) from the error.
  (If VAR is nil, the handler can't access that information.)
  Then the value of the last BODY form is returned from the `condition-case'
  expression.

  See also the function `signal' for more info."
  {:arglists '([VAR BODYFORM &rest HANDLERS])}
  [var bodyform & handlers]
  `(try
     ~bodyform
     (catch EmacsLispError e#
       (c/let [~(if var var (gensym "_")) (.data e#)]
         (case (.symbol e#)
           ~@(apply concat (for [[c & h] handlers]
                             `[~c (do ~@h)]))
           (throw e#))))))

(c/defmacro cond
  "Try each clause until one succeeds.
  Each clause looks like (CONDITION BODY...).  CONDITION is evaluated
  and, if the value is non-nil, this clause succeeds:
  then the expressions in BODY are evaluated and the last one's
  value is the value of the cond-form.
  If no clause succeeds, cond returns nil.
  If a clause has one element, as in (CONDITION),
  CONDITION's value if non-nil is returned from the cond-form."
  {:arglists '([CLAUSES...])}
  [& clauses]
  `(c/cond ~@(apply concat clauses)))

(defn ^:private first-symbol [s]
  (if (symbol? s) s
      (loop [s s]
        (if (symbol? (second s))
          (second s)
          (recur (second s))))))

(c/defmacro setq-helper* [locals sym-vals]
  `(c/let
       ~(reduce into []
                (for [[s v] (partition 2 sym-vals)
                      :let [s (if (seq? s) (second s) s)]]
                  [(symbol (name (first-symbol s)))
                   (if (contains? locals s)
                     `(do (reset! ~s ~v) ~s)
                     `(if-let [var# (ns-resolve 'deuce.emacs-lisp.globals '~s)]
                        (if (contains? (get-thread-bindings) var#)
                          (var-set var# ~v)
                          (alter-var-root var# (constantly ~v)))
                        (do
                          (defvar ~s ~v)
                          ~v)))]))
     ~(last (butlast sym-vals))))

(c/defmacro setq
  "Set each SYM to the value of its VAL.
  The symbols SYM are variables; they are literal (not evaluated).
  The values VAL are expressions; they are evaluated.
  Thus, (setq x (1+ y)) sets `x' to the value of `(1+ y)'.
  The second VAL is not computed until after the first SYM is set, and so on;
  each VAL can use the new value of variables set earlier in the `setq'.
  The return value of the `setq' form is the value of the last VAL."
  {:arglists '([[SYM VAL]...])}
  [& sym-vals]
  `(setq-helper* ~(c/set (keys &env)) ~sym-vals))

(c/defmacro ^:clojure-special-form quote
  "Return the argument, without evaluating it.  `(quote x)' yields `x'.
  Warning: `quote' does not construct its return value, but just returns
  the value that was pre-constructed by the Lisp reader (see info node
  `(elisp)Printed Representation').
  This means that '(a . b) is not identical to (cons 'a 'b): the former
  does not cons.  Quoting should be reserved for constants that will
  never be modified by side-effects, unless you like self-modifying code.
  See the common pitfall in info node `(elisp)Rearrangement' for an example
  of unexpected results when a quoted object is modified."
  {:arglists '([ARG])}
  [arg]
  `(quote ~arg))

(c/defmacro let-helper* [can-refer? varlist & body]
  (c/let [varlist (->> varlist
                       (map #(if (symbol? %) [% nil] %))
                       (map (fn [[s v]] [(first-symbol s) v])))
          {:keys [lexical dynamic]} (group-by (comp #(if (namespace %) :dynamic :lexical) first) varlist)
          lexical-vars (into {} (map vec lexical))
          dynamic-vars (into {} (map vec dynamic))
          fix-lexical-setq (fn [form] (c/cond
                                        (c/and (seq? form) (symbol? (first form))
                                               (= 'setq (symbol (name (first form)))))
                                        (list 'deuce.emacs-lisp/setq-helper* (c/set (keys lexical-vars)) (rest form))

                                        (c/and (seq? form) (symbol? (first form))
                                               (= 'setq-helper* (symbol (name (first form)))))
                                        (concat ['deuce.emacs-lisp/setq-helper* (into (second form) (keys lexical-vars))] (drop 2 form))

                                        :else form))
          body (w/postwalk fix-lexical-setq
                           (w/postwalk-replace (zipmap (keys lexical-vars)
                                                       (map #(list 'clojure.core/deref %) (keys lexical-vars))) body))
          all-vars (map first varlist)
          temps (zipmap all-vars (repeatedly #(gensym "local")))
          lexical-vars (if can-refer? lexical-vars (select-keys temps (keys lexical-vars)))
          dynamic-vars (if can-refer? dynamic-vars (select-keys temps (keys dynamic-vars)))]
    `(c/let ~(if can-refer? [] (vec (interleave (map temps (map first varlist)) (map second varlist))))
       ~((fn build-let [[v & vs]]
           (if v
             (if-let [local (lexical-vars v)]
               `(c/let [~v (atom ~local)] ~(build-let vs))
               `(c/binding [~v ~(dynamic-vars v)] ~(build-let vs)))
             `(do ~@body))) all-vars))))

(c/defmacro let
  "Bind variables according to VARLIST then eval BODY.
  The value of the last form in BODY is returned.
  Each element of VARLIST is a symbol (which is bound to nil)
  or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
  All the VALUEFORMs are evalled before any symbols are bound."
  {:arglists '([VARLIST BODY...])}
  [varlist & body]
  `(let-helper* false ~varlist ~@body))

(c/defmacro defconst
  "Define SYMBOL as a constant variable.
  This declares that neither programs nor users should ever change the
  value.  This constancy is not actually enforced by Emacs Lisp, but
  SYMBOL is marked as a special variable so that it is never lexically
  bound.

  The `defconst' form always sets the value of SYMBOL to the result of
  evalling INITVALUE.  If SYMBOL is buffer-local, its default value is
  what is set; buffer-local values are not affected.  If SYMBOL has a
  local binding, then this form sets the local binding's value.
  However, you should normally not make local bindings for variables
  defined with this form.

  The optional DOCSTRING specifies the variable's documentation string."
  {:arglists '([SYMBOL INITVALUE [DOCSTRING]])}
  [symbol initvalue & [docstring]]
  (c/let [symbol (c/symbol (name symbol))]
    `(do
       (-> (intern (create-ns 'deuce.emacs-lisp.globals)
                   '~symbol
                   ~initvalue)
           (alter-meta! merge {:doc ~(apply str docstring)}))
       '~symbol)))

(c/defmacro prog1
  "Eval FIRST and BODY sequentially; return value from FIRST.
  The value of FIRST is saved during the evaluation of the remaining args,
  whose values are discarded."
  {:arglists '([FIRST BODY...])}
  [first & body]
  `(c/let [result# ~first]
     ~@body
     result#))

(c/defmacro prog2
  "Eval FORM1, FORM2 and BODY sequentially; return value from FORM2.
  The value of FORM2 is saved during the evaluation of the
  remaining args, whose values are discarded."
  {:arglists '([FORM1 FORM2 BODY...])}
  [form1 form2 & body]
  `(do ~form1
       (prog1 ~form2
              ~@body)))

(c/defmacro setq-default
  "Set the default value of variable VAR to VALUE.
  VAR, the variable name, is literal (not evaluated);
  VALUE is an expression: it is evaluated and its value returned.
  The default value of a variable is seen in buffers
  that do not have their own values for the variable.

  More generally, you can use multiple variables and values, as in
    (setq-default VAR VALUE VAR VALUE...)
  This sets each VAR's default value to the corresponding VALUE.
  The VALUE for the Nth VAR can refer to the new default values
  of previous VARs."
  {:arglists '([[VAR VALUE]...])}
  [& var-values]
  `(setq ~@var-values))

(c/defmacro or
  "Eval args until one of them yields non-nil, then return that value.
  The remaining args are not evalled at all.
  If all args return nil, return nil."
  {:arglists '([CONDITIONS...])}
  [& conditions]
  `(c/or ~@conditions))

(c/defmacro while
  "If TEST yields non-nil, eval BODY... and repeat.
  The order of execution is thus TEST, BODY, TEST, BODY and so on
  until TEST returns nil."
  {:arglists '([TEST BODY...])}
  [test & body]
  `(c/while ~test ~@body))

(c/defmacro defmacro
  "Define NAME as a macro.
  The actual definition looks like
   (macro lambda ARGLIST [DOCSTRING] [DECL] BODY...).
  When the macro is called, as in (NAME ARGS...),
  the function (lambda ARGLIST BODY...) is applied to
  the list ARGS... as it appears in the expression,
  and the result should be a form to be evaluated instead of the original.

  DECL is a declaration, optional, which can specify how to indent
  calls to this macro, how Edebug should handle it, and which argument
  should be treated as documentation.  It looks like this:
    (declare SPECS...)
  The elements can look like this:
    (indent INDENT)
  	Set NAME's `lisp-indent-function' property to INDENT.

    (debug DEBUG)
  	Set NAME's `edebug-form-spec' property to DEBUG.  (This is
  	equivalent to writing a `def-edebug-spec' for the macro.)

    (doc-string ELT)
  	Set NAME's `doc-string-elt' property to ELT."
  {:arglists '([NAME ARGLIST [DOCSTRING] [DECL] BODY...])}
  [name arglist & body]
  `(do
     ~(when-not ((ns-interns 'deuce.emacs-lisp) name)
        `(def-helper* c/defmacro ~name ~arglist ~@body))
     '~name))

(c/defmacro function
  "Like `quote', but preferred for objects which are functions.
  In byte compilation, `function' causes its argument to be compiled.
  `quote' cannot do that."
  {:arglists '([ARG])}
  [arg]
  `(quote ~arg))

(c/defmacro and
  "Eval args until one of them yields nil, then return nil.
  The remaining args are not evalled at all.
  If no arg yields nil, return the last arg's value."
  {:arglists '([CONDITIONS...])}
  [& conditions]
  `(c/and ~@conditions))

(c/defmacro progn
  "Eval BODY forms sequentially and return value of last one."
  {:arglists '([BODY...])}
  [& body]
  `(do ~@body))

(c/defmacro ^:clojure-special-form let*
  "Bind variables according to VARLIST then eval BODY.
  The value of the last form in BODY is returned.
  Each element of VARLIST is a symbol (which is bound to nil)
  or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
  Each VALUEFORM can refer to the symbols already bound by this VARLIST."
  {:arglists '([VARLIST BODY...])}
  [varlist & body]
  (if (vector? varlist)
    `(c/let ~varlist ~@body)
    `(let-helper* true ~varlist ~@body)))

(defn defvar-helper* [ns symbol & [initvalue docstring]]
  (c/let [symbol (c/symbol (name (first-symbol symbol)))]
    (do
      (->
       (intern (create-ns ns)
               symbol
               initvalue)
       .setDynamic
       (alter-meta! merge {:doc (apply str docstring)}))
      symbol)))

(c/defmacro defvar
  "Define SYMBOL as a variable, and return SYMBOL.
  You are not required to define a variable in order to use it, but
  defining it lets you supply an initial value and documentation, which
  can be referred to by the Emacs help facilities and other programming
  tools.  The `defvar' form also declares the variable as \"special\",
  so that it is always dynamically bound even if `lexical-binding' is t.

  The optional argument INITVALUE is evaluated, and used to set SYMBOL,
  only if SYMBOL's value is void.  If SYMBOL is buffer-local, its
  default value is what is set; buffer-local values are not affected.
  If INITVALUE is missing, SYMBOL's value is not set.

  If SYMBOL has a local binding, then this form affects the local
  binding.  This is usually not what you want.  Thus, if you need to
  load a file defining variables, with this form or with `defconst' or
  `defcustom', you should always load that file _outside_ any bindings
  for these variables.  (`defconst' and `defcustom' behave similarly in
  this respect.)

  The optional argument DOCSTRING is a documentation string for the
  variable.

  To define a user option, use `defcustom' instead of `defvar'.
  The function `user-variable-p' also identifies a variable as a user
  option if its DOCSTRING starts with *, but this behavior is obsolete."
  {:arglists '([SYMBOL &optional INITVALUE DOCSTRING])}
  [symbol & [initvalue docstring]]
  `(defvar-helper* 'deuce.emacs-lisp.globals '~symbol ~initvalue ~docstring))

;; defined as fn in eval.clj
(c/defmacro ^:clojure-special-form throw
  "Throw to the catch for TAG and return VALUE from it.
  Both TAG and VALUE are evalled."
  {:arglists '([TAG VALUE])}
  [tag value]
  `(throw (EmacsLispError. ~tag ~value)))

(c/defmacro ^:clojure-special-form catch
  "Eval BODY allowing nonlocal exits using `throw'.
  TAG is evalled to get the tag to use; it must not be nil.

  Then the BODY is executed.
  Within BODY, a call to `throw' with the same TAG exits BODY and this `catch'.
  If no throw happens, `catch' returns the value of the last BODY form.
  If a throw happens, it specifies the value to return from `catch'."
  {:arglists '([TAG BODY...])}
  [tag & body]
  `(try
     ~@body
     (catch EmacsLispError e#
       (if (= ~tag (.symbol e#))
         (.data e#)
         (throw e#)))))

(c/defmacro ^:clojure-special-form if
  "If COND yields non-nil, do THEN, else do ELSE...
  Returns the value of THEN or the value of the last of the ELSE's.
  THEN must be one expression, but ELSE... can be zero or more expressions.
  If COND yields nil, and there are no ELSE's, the value is nil."
  {:arglists '([COND THEN ELSE...])}
  [cond then & else]
  `(c/cond ~cond ~then
           :else (do ~@else)))

(c/defmacro save-restriction
  "Execute BODY, saving and restoring current buffer's restrictions.
  The buffer's restrictions make parts of the beginning and end invisible.
  (They are set up with `narrow-to-region' and eliminated with `widen'.)
  This special form, `save-restriction', saves the current buffer's restrictions
  when it is entered, and restores them when it is exited.
  So any `narrow-to-region' within BODY lasts only until the end of the form.
  The old restrictions settings are restored
  even in case of abnormal exit (throw or error).

  The value returned is the value of the last form in BODY.

  Note: if you are using both `save-excursion' and `save-restriction',
  use `save-excursion' outermost:
      (save-excursion (save-restriction ...))"
  {:arglists '([&rest BODY])}
  [& body])

(c/defmacro save-window-excursion
  "Execute BODY, preserving window sizes and contents.
  Return the value of the last form in BODY.
  Restore which buffer appears in which window, where display starts,
  and the value of point and mark for each window.
  Also restore the choice of selected window.
  Also restore which buffer is current.
  Does not restore the value of point in current buffer."
  {:arglists '([BODY...])}
  [& body])

(c/defmacro save-excursion
  "Save point, mark, and current buffer; execute BODY; restore those things.
  Executes BODY just like `progn'.
  The values of point, mark and the current buffer are restored
  even in case of abnormal exit (throw or error).
  The state of activation of the mark is also restored.

  This construct does not save `deactivate-mark', and therefore
  functions that change the buffer will still cause deactivation
  of the mark at the end of the command.  To prevent that, bind
  `deactivate-mark' with `let'.

  If you only want to save the current buffer but not point nor mark,
  then just use `save-current-buffer', or even `with-current-buffer'."
  [& body])

(c/defmacro with-output-to-temp-buffer
  "Bind `standard-output' to buffer BUFNAME, eval BODY, then show that buffer."
  {:arglists '([BUFNAME BODY...])}
  [bufname & body])

(c/defmacro interactive
  "Specify a way of parsing arguments for interactive use of a function.
  For example, write
   (defun foo (arg buf) \"Doc string\" (interactive \"P\\nbbuffer: \") .... )
   to make ARG be the raw prefix argument, and set BUF to an existing buffer,
   when `foo' is called as a command.
  The \"call\" to `interactive' is actually a declaration rather than a function;
   it tells `call-interactively' how to read arguments
   to pass to the function.
  When actually called, `interactive' just returns nil.

  Usually the argument of `interactive' is a string containing a code letter
   followed optionally by a prompt.  (Some code letters do not use I/O to get
   the argument and do not use prompts.)  To get several arguments, concatenate
   the individual strings, separating them by newline characters.
  Prompts are passed to format, and may use % escapes to print the
   arguments that have already been read.
  If the argument is not a string, it is evaluated to get a list of
   arguments to pass to the function.
  Just `(interactive)' means pass no args when calling interactively.

  Code letters available are:
  a -- Function name: symbol with a function definition.
  b -- Name of existing buffer.
  B -- Name of buffer, possibly nonexistent.
  c -- Character (no input method is used).
  C -- Command name: symbol with interactive function definition.
  d -- Value of point as number.  Does not do I/O.
  D -- Directory name.
  e -- Parameterized event (i.e., one that's a list) that invoked this command.
       If used more than once, the Nth `e' returns the Nth parameterized event.
       This skips events that are integers or symbols.
  f -- Existing file name.
  F -- Possibly nonexistent file name.
  G -- Possibly nonexistent file name, defaulting to just directory name.
  i -- Ignored, i.e. always nil.  Does not do I/O.
  k -- Key sequence (downcase the last event if needed to get a definition).
  K -- Key sequence to be redefined (do not downcase the last event).
  m -- Value of mark as number.  Does not do I/O.
  M -- Any string.  Inherits the current input method.
  n -- Number read using minibuffer.
  N -- Numeric prefix arg, or if none, do like code `n'.
  p -- Prefix arg converted to number.  Does not do I/O.
  P -- Prefix arg in raw form.  Does not do I/O.
  r -- Region: point and mark as 2 numeric args, smallest first.  Does no I/O.
  s -- Any string.  Does not inherit the current input method.
  S -- Any symbol.
  U -- Mouse up event discarded by a previous k or K argument.
  v -- Variable name: symbol that is user-variable-p.
  x -- Lisp expression read but not evaluated.
  X -- Lisp expression read and evaluated.
  z -- Coding system.
  Z -- Coding system, nil if no prefix arg.

  In addition, if the string begins with `*', an error is signaled if
    the buffer is read-only.
  If `@' appears at the beginning of the string, and if the key sequence
   used to invoke the command includes any mouse events, then the window
   associated with the first of those events is selected before the
   command is run.
  If the string begins with `^' and `shift-select-mode' is non-nil,
   Emacs first calls the function `handle-shift-selection'.
  You may use `@', `*', and `^' together.  They are processed in the
   order that they appear, before reading any arguments."
  {:arglists '([&optional ARGS])}
  [& args])

(c/defmacro save-current-buffer
  "Save the current buffer; execute BODY; restore the current buffer.
  Executes BODY just like `progn'."
  {:arglists '([&rest BODY])}
  [& body])

(eval `(setq ~(symbol "nil") nil))
(setq t true)

(setq motif-version-string "")
(setq gtk-version-string "")
(setq ns-version-string "")
(setq x-toolkit-scroll-bars nil)
(setq msdos-long-file-names nil)

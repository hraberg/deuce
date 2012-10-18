(ns deuce.emacs-lisp
  (:require [clojure.core :as c]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.walk :as w])
  (:use [taoensso.timbre :as timbre
         :only (trace debug info warn error fatal spy)])
  (:import [clojure.lang Atom]
           [deuce EmacsLispError DottedPair]
           [java.util LinkedList List])
  (:refer-clojure :exclude [defmacro and or cond let while eval set compile]))

(timbre/set-config! [:prefix-fn]
                    (fn [{:keys [level timestamp hostname ns]}]
                      (str timestamp " " (-> level name s/upper-case) " [" ns "]")))
(timbre/set-config! [:timestamp-pattern] "HH:mm:ss,SSS")

(timbre/set-level! :debug)

(create-ns 'deuce.emacs)
(create-ns 'deuce.emacs-lisp.globals)

(declare clojure-special-forms)

(defn sym [s]
  (symbol (name s)))

; "reused" from data.clj
(defn not-null? [object]
  (when-not (c/or (nil? object) (c/= () object) (false? object))
    object))

(defn first-symbol [form]
  (when ((every-pred (some-fn seq? list?) (comp symbol? first)) form)
    (first form)))

(defn nested-first-symbol [s]
  (if ((some-fn symbol? (complement seq?)) s)
    s
    (when s
      (recur (second s)))))

(defn global [s]
  (ns-resolve 'deuce.emacs-lisp.globals (sym s)))

(defn fun [s]
  (if (fn? s) s
      (ns-resolve 'deuce.emacs (sym s))))

(defn maybe-sym [x]
  (if (symbol? x) (sym x) x))

(defn qualify-globals [locals form]
  (if-let [s (c/and (symbol? form)
                    (not (locals form))
                    (not (re-find #"\." (name form)))
                    ((some-fn clojure-special-forms global) (sym form)))]
    (symbol (-> s meta :ns str) (-> s meta :name str))
    form))

(defn qualify-fns [form]
  (if-let [s (when-let [s (first-symbol form)]
               (c/and
                (not= "clojure.core" (namespace s))
                (fun s)))]
    (cons (symbol (-> s meta :ns str) (-> s meta :name str)) (next form))
    (if (c/and (symbol? form) (= "deuce.emacs" (namespace form)))
      (sym form)
      form)))

(defn strip-comments [form]
  (if (seq? form)
    (remove (every-pred seq? (comp `#{comment} first)) form)
    form))

(defn expand-dotted-pairs [form]
  (if (c/and (seq? form) (= 3 (count form)) (= '. (second form)))
    (DottedPair. (first form) (last form))
    form))

(defn vectors-to-arrays [form]
  (if (vector? form)
    (object-array form)
    form))

; doesn't work as intended
(defn lists-to-linked-lists [form]
  (if (c/and (seq? form) (= 'quote (first form))
             (seq? (second form)))
    (list 'quote  (LinkedList. (second form)))
    form))

(defn protect-forms [form]
  (if ('#{defun defmacro} (maybe-sym (first-symbol form)))
    ^:protect-from-expansion (fn [] form)
    form))

(defn unprotect-forms [form]
  (if (-> form meta :protect-from-expansion) (form) form))

(defn preprocess [scope body]
  (with-meta
    `(fn ~(vec scope)
       ~(->> body
             (w/postwalk (comp strip-comments
                               expand-dotted-pairs
                               vectors-to-arrays
                               protect-forms))
             (w/postwalk (comp unprotect-forms
                               qualify-fns
                               (partial qualify-globals (c/set scope))))))
    (merge (meta body) {:scope scope :src body})))

(defn cause [e]
  (if-let [e (.getCause e)]
    (recur e)
    e))

(defn pprint-arg [arg]
  (c/let [arg (s/trim-newline (with-out-str (pp/pprint arg)))]
    (if (re-find #"\n" arg) (str "\n" arg) arg)))

(defn compile [emacs-lisp]
  (try
    (c/eval (with-meta emacs-lisp nil))
    (catch RuntimeException e
      (if-let [[_ undeclared] (c/and (global 'load-in-progress)
                                     @(global 'load-in-progress)
                                     (c/re-find #"Unable to resolve symbol: (.+) in this context"
                                                (.getMessage e)))]
        (do
          (warn (-> e cause .getMessage))
          (intern 'deuce.emacs (symbol undeclared))
          (compile emacs-lisp))
        (do
          (error (-> e cause .getMessage) (pprint-arg emacs-lisp))
          (throw e))))))

(defn limit-scope [scope]
  (->> scope
       (remove '#{&env &form})
       (remove #(re-find #"\w__\d+" (name %)))
       seq))

;; defined as fn in eval.clj
(c/defmacro eval
  "Evaluate FORM and return its value.
  If LEXICAL is t, evaluate using lexical scoping."
  {:arglists '([FORM &optional LEXICAL])}
  [body & [lexical]]
  (c/let [scope (limit-scope (keys &env))]
    `(binding [*ns* (the-ns 'deuce.emacs)]
       (maybe-sym ((compile (preprocess '~scope ~body)) ~@scope)))))

(declare let-helper*)

(defn linked-lists-to-seqs [form]
  (if (instance? List form)
    (apply clojure.core/list form)
    form))

(c/defmacro trace-indent [& args]
  `(c/let [depth# (->> (.getStackTrace (Thread/currentThread))
                       (filter #(re-find #"deuce." (str %)))
                       count)]
     (trace (apply str (repeat depth# "-")) ~@args)))

(defn underef [form]
  (if (c/and (seq? form) (= `deref (first form)))
    (nested-first-symbol form)
    form))

(declare progn)

(c/defmacro def-helper* [what line name arglist & body]
  (c/let [[docstring body] (split-with string? body)
          name (sym (if (seq? name) (c/eval name) name))
          rest-arg (maybe-sym (second (drop-while (complement '#{&rest}) arglist)))
          [arg & args :as arglist] (replace '{&rest &} arglist)
          [arglist &optional optional-args] (if (= '&optional arg)
                                              [() arg args]
                                              (partition-by '#{&optional} arglist))
          arglist (concat arglist (when &optional ['& (vec optional-args)]))
          [[interactive] body] (split-with #(c/and (seq? %)
                                                   (= 'interactive (first %))) body)
          emacs-lisp? (= (the-ns 'deuce.emacs) *ns*)
          doc (apply str docstring)
          arglist (w/postwalk maybe-sym arglist)
          the-args (remove '#{&} (flatten arglist))]
    `(c/let [f# (~what ~name ~(vec arglist)
                       ~(when-not (seq body) `(warn ~(c/name name) "NOT IMPLEMENTED"))
                       (binding [*ns* (the-ns 'clojure.core)]
                         (trace-indent '~name)
                         ~@(for [arg the-args]
                             `(trace ~(keyword arg) (pprint-arg ~arg))))
                       (c/let [result# ~(if emacs-lisp?
                                          `(c/let ~(if rest-arg `[~rest-arg (if-let [r# (seq ~rest-arg)] (LinkedList. r#) (LinkedList.))] [])
                                             (if (= '~'defmacro '~(sym what))
                                               (c/let [expansion# (let-helper* false ~(map #(list % %) the-args)
                                                                    (eval '(progn ~@body)))]
                                                 (w/prewalk linked-lists-to-seqs expansion#))
                                               (let-helper* false ~(map #(list % %) the-args)
                                                 (eval '(progn ~@body)))))
                                          `(do ~@body))]

                         (binding [*ns* (the-ns 'clojure.core)]
                           (trace-indent '~name "⇒" (pprint-arg result#)))

                         result#))]
       (if (var? f#)
         (do
           (alter-meta! f# merge (merge {:doc ~doc}
                                        (when ~emacs-lisp?
                                          {:line ~line
                                           :file (when-let [file# (global 'load-file-name)]
                                                   @file#)})))
           (alter-var-root f# (constantly (with-meta @f# (meta f#)))))
         (with-meta f# (assoc (meta f#) :doc ~doc))))))

(c/defmacro defun
  "Define NAME as a function.
  The definition is (lambda ARGLIST [DOCSTRING] BODY...).
  See also the function `interactive'."
  {:arglists '([NAME ARGLIST [DOCSTRING] BODY...])}
  [name arglist & body]
  `(do (def-helper* defn ~(-> name meta :line) ~name ~arglist ~@body)
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
  `(def-helper* fn nil lambda ~(first cdr) ~@(rest cdr)))

(c/defmacro unwind-protect
  "Do BODYFORM, protecting with UNWINDFORMS.
  If BODYFORM completes normally, its value is returned
  after executing the UNWINDFORMS.
  If BODYFORM exits nonlocally, the UNWINDFORMS are executed anyway."
  {:arglists '([BODYFORM UNWINDFORMS...])}
  [bodyform & unwindforms]
  `(try
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
                             `[~(sym c) (do ~@h)]))
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
  `(c/cond
     ~@(->> clauses
            (map #(do [`(not-null? ~(first %)) (if (second %) `(progn ~@(rest %)) (first %))]))
            (apply concat))))

(c/defmacro setq-helper* [locals default? sym-vals]
  `(c/let
       ~(reduce into []
                (for [[s v] (partition 2 sym-vals)
                      :let [s (nested-first-symbol s)]]
                  [(sym s)
                   (if (contains? locals s)
                     `(do (reset! ~s ~v) ~s)
                     `(if-let [var# (global '~s)]
                        (if (c/and (contains? (get-thread-bindings) var#)
                                   (not ~default?))
                          (var-set var# ~v)
                          (c/let [m# (meta var#)]
                            (alter-var-root var# (constantly ~v))
                            (alter-meta! var# (constantly m#))
                            @var#))
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
  `(setq-helper* ~(c/set (keys &env)) false ~sym-vals))

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

(defn fix-lexical-setq [lexical-vars form]
  (if (lexical-vars form)
    (list `deref form)
    (condp some [(maybe-sym (first-symbol form))]
      '#{setq} (list `setq-helper*
                     lexical-vars false (rest form))
      '#{setq-helper*} (concat [`setq-helper*
                                (into (second form) lexical-vars)] (drop 2 form))
      '#{deref} (list `deref (nested-first-symbol form))
      form)))

(c/defmacro let-helper* [can-refer? varlist & body]
  (c/let [varlist (map #(if (symbol? %) [% nil] %) varlist)
          fn-vars (->> varlist
                       (map first)
                       (filter namespace)
                       (remove (comp #{"deuce.emacs-lisp.globals"} namespace)))
          fn-vars-fix (merge (zipmap fn-vars (map sym fn-vars))
                             (zipmap (map #(symbol "deuce.emacs-lisp.globals" (name %)) fn-vars)
                                     (map sym fn-vars)))
          varlist (map (fn [[s v]] [(c/let [s (nested-first-symbol s)]
                                      (fn-vars-fix s s)) v]) varlist)
          {:keys [lexical dynamic]} (group-by (comp #(if (namespace %) :dynamic :lexical) first) varlist)
          lexical-vars (into {} (map vec lexical))
          dynamic-vars (into {} (map vec dynamic))
          fix-lexical-setq (partial fix-lexical-setq (c/set (keys lexical-vars)))
          body (w/postwalk fix-lexical-setq body)
          body (w/postwalk-replace fn-vars-fix body)
          all-vars (map first varlist)
          temps (zipmap all-vars (repeatedly #(gensym "local__")))
          [lexical-vars dynamic-vars] (map #(if can-refer? % (select-keys temps (keys %)))
                                           [lexical-vars dynamic-vars])]
    `(c/let ~(if can-refer? [] (vec (interleave (map temps (map first varlist)) (map second varlist))))
       ~((fn build-let [[v & vs]]
           (if v
             (if (contains? lexical-vars v)
               `(c/let [~v (atom ~(w/postwalk fix-lexical-setq (lexical-vars v)))]
                  ~(build-let vs))
               `(c/binding [~v ~(dynamic-vars v)]
                  ~(build-let vs)))
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
  (c/let [symbol (sym symbol)]
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
  `(setq-helper* #{} :default ~var-values))

(c/defmacro or
  "Eval args until one of them yields non-nil, then return that value.
  The remaining args are not evalled at all.
  If all args return nil, return nil."
  {:arglists '([CONDITIONS...])}
  [& conditions]
  `(c/or ~@(map #(do `(not-null? ~%)) conditions)))

(c/defmacro while
  "If TEST yields non-nil, eval BODY... and repeat.
  The order of execution is thus TEST, BODY, TEST, BODY and so on
  until TEST returns nil."
  {:arglists '([TEST BODY...])}
  [test & body]
  `(c/while (not-null? ~test) ~@body))

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
        `(def-helper* c/defmacro ~(-> name meta :line) ~name ~arglist ~@body))
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
  `(c/and ~@(map #(do `(not-null? ~%)) conditions)))

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
  (c/let [symbol (sym (nested-first-symbol symbol))
          default (global symbol)
          m (meta default)]
    (->
     (intern (create-ns ns)
             symbol
             (c/or (when default
                     (.getRawRoot default))
                   initvalue))
     .setDynamic
     (alter-meta! merge (merge m {:doc (apply str docstring)})))
    symbol))

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
  `(c/cond (not-null? ~cond) ~then
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
  [& body]
  `(do ~@body))

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
  [& body]
  `(do ~@body))

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
  [& body]
  `(do ~@body))

(def clojure-special-forms
  (->> (ns-map 'deuce.emacs-lisp)
       (filter (comp :clojure-special-form meta val))
       (into {})))

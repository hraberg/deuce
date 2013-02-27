(ns deuce.emacs-lisp
  (:require [clojure.core :as c]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.walk :as w]
            [deuce.emacs-lisp.cons :as cons])
  (:use [taoensso.timbre :as timbre
         :only (trace debug info warn error fatal spy)])
  (:import [clojure.lang Atom Box Var]
           [deuce.emacs_lisp.error]
           [deuce.emacs_lisp.cons Cons]
           [java.util List])
  (:refer-clojure :exclude [defmacro and or cond let while eval set compile]))

(timbre/set-config! [:prefix-fn]
                    (fn [{:keys [level timestamp hostname ns]}]
                      (str timestamp " " (-> level name s/upper-case) " [" ns "]")))
(timbre/set-config! [:timestamp-pattern] "HH:mm:ss,SSS")

(timbre/set-level! :debug)
(var-set #'*warn-on-reflection* true)

(create-ns 'deuce.emacs)
(create-ns 'deuce.emacs-lisp.globals)

(declare clojure-special-forms throw defvar)

(defn sym [s]
  (symbol (name s)))

; "reused" from data.clj
(defn not-null? [object]
  (when-not (c/or (nil? object) (c/= () object) (false? object))
    object))

(defn global [s]
  (ns-resolve 'deuce.emacs-lisp.globals (sym s)))

(defn fun [s]
  (if (fn? s) s
      (ns-resolve 'deuce.emacs (sym s))))

(defn maybe-sym [x]
  (if (symbol? x) (sym x) x))

(defn expand-symbol [x]
  (sym (if (seq? x) (c/eval x) x)))

;; New Dynamic Binding Logic - still to be integrated and tested. Fully Var based.
(def ^:dynamic *dynamic-vars* {})

(c/defmacro el-var-get [name]
  `(if-let [v# ((some-fn *dynamic-vars* global) '~name)]
     @v#
     ~(if (c/and (symbol? name) (name &env))
        `(do ~name)
        `(deuce.emacs-lisp/throw '~'void-variable (cons/list '~name)))))

(c/defmacro el-var-set [name value]
  `(if-let [v# (*dynamic-vars* '~name)]
     (if (c/and (.hasRoot v#) (not (.getThreadBinding v#)))
       (alter-var-root v# (constantly ~value))
       (var-set v# ~value))
     (if-let [v# (global '~name)]
       (alter-var-root v# (constantly ~value))
       @(global (defvar ~name ~value)))))

(c/defmacro with-local-el-vars [name-vals-vec & body]
  (c/let [vars (vec (take-nth 2 name-vals-vec))]
         `(c/let [vars# (hash-map ~@(interleave (map #(list 'quote %) (take-nth 2 name-vals-vec))
                                                (repeat '(c/doto (clojure.lang.Var/create) .setDynamic))))]
                 (with-bindings (zipmap (map vars# '~vars) ~(vec (take-nth 2 (rest name-vals-vec))))
                   (binding [*dynamic-vars* (merge *dynamic-vars* vars#)]
                     ~@body)))))

(defn el->clj [x]
  (condp some [x]
    #{()} nil
    seq? (c/let [[fst & rst] x]
                (if (c/and (symbol? fst)
                           (not= 'progn fst)
                           (-> (fun fst) meta :macro))
                  (if (clojure-special-forms fst)
                    (if (= 'quote fst)
                      (if (c/and (symbol? (first rst)) (not (next rst)))
                        (list 'quote (sym (first rst)))
                        x)
                      (cons (symbol "deuce.emacs-lisp" (name fst)) rst))
                    x)
                  (if (#{`el-var-get `el-var-set} fst)
                    x
                    (cons (c/cond
                           (c/and (symbol? fst)
                                  (not (namespace fst))
                                  (not (fun fst)))
                           (do (warn (format "Unable to resolve symbol: %s in this context" fst))
                               (intern 'deuce.emacs fst)
                               (list `fun (list 'quote fst)))

                           (c/and (c/seq? fst) (= `symbol (first fst)))
                           (list `fun fst)

                           :else fst)
                          (map el->clj rst)))))
    symbol? (if (namespace x) x (list `el-var-get x))
    x))
;; New Dynamic Binding Logic End

(defn ^Throwable cause [^Throwable e]
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

;; Navgeet's helper macro, will revisit, basically condition-case but for use from Clojure
(c/defmacro try-with-tag [& exprs]
  (c/let [catch-clauses (c/filter #(c/= (first %) 'catch) exprs)
          finally-clause (c/filter #(c/= (first %) 'finally) exprs)
          try-exprs (c/remove #(c/or (c/= (first %) 'finally) (c/= (first %) 'catch)) exprs)]
         `(try ~@try-exprs
               ~@(for [expr catch-clauses]
                   (c/let [[_ tag e & exprs] expr]
                          `(catch deuce.emacs_lisp.error e#
                             (if (= ~tag (:symbol (.state e#)))
                               (c/let [~e e#]
                                      (do ~@exprs))
                               (throw e#)))))
               ~@finally-clause)))

;; defined as fn in eval.clj
(c/defmacro eval
  "Evaluate FORM and return its value.
  If LEXICAL is t, evaluate using lexical scoping."
  {:arglists '([FORM &optional LEXICAL])}
  [body & [lexical]]
  `(binding [*ns* (the-ns 'deuce.emacs)]
     (maybe-sym (compile ~(el->clj body)))))

(declare let-helper*)

(defn cons-lists-to-seqs [form]
  (if (instance? Cons form)
    (apply list form)
    form))

(c/defmacro trace-indent [& args]
  `(trace (apply str (repeat (->> (.getStackTrace (Thread/currentThread))
                                  (filter #(re-find #"deuce." (str %)))
                                  count) "-")) ~@args))

(declare progn)

(c/defmacro def-helper* [what line name arglist & body]
  (c/let [[docstring body] (split-with string? body)
          name (expand-symbol name)
          rest-arg (maybe-sym (second (drop-while (complement '#{&rest}) arglist)))
          [arg & args :as arglist] (map expand-symbol (replace '{&rest &} arglist))
          [arglist &optional optional-args] (if (= '&optional arg)
                                              [() arg args]
                                              (partition-by '#{&optional} arglist))
          arglist (concat arglist (when &optional ['& (vec optional-args)]))
          [[interactive] body] (split-with #(c/and (seq? %)
                                                   (= 'interactive (first %))) body)
          emacs-lisp? (= (the-ns 'deuce.emacs) *ns*)
          macro? (= `c/defmacro what)
          doc (apply str docstring)
          arglist (w/postwalk maybe-sym arglist)
          the-args (remove '#{&} (flatten arglist))]
    `(c/let [f# (~what ~name ~(vec arglist)
                       (binding [*ns* (the-ns 'clojure.core)]
;                         (trace-indent '~name)
                         ~(when-not (seq body) `(warn ~(c/name name) "NOT IMPLEMENTED"))
                         ;; ~@(for [arg the-args]
                         ;;     `(trace ~(keyword arg) (pprint-arg ~arg)))
                         )
                       (c/let [result# ~(if emacs-lisp?
                                          `(c/let ~(if rest-arg
                                                     `[~rest-arg (if-let [r# (seq ~rest-arg)] (apply cons/list r#) nil)]
                                                     [])
                                                  (c/let [result# (with-local-el-vars ~(vec (mapcat #(c/list % %) the-args))
                                                                    (progn ~@body))]
                                                         (if ~macro?
                                                           (el->clj (w/prewalk cons-lists-to-seqs result#))
                                                           result#)))
                                          `(do ~@body))]

                         ;; (binding [*ns* (the-ns 'clojure.core)]
                         ;;   (trace-indent '~name "⇒" (pprint-arg result#)))

                         result#))]
       (if (var? f#)
         (do
           (alter-meta! f# merge (merge {:doc ~doc}
                                        (when ~emacs-lisp?
                                          {:el-file (when-let [file# (global 'load-file-name)]
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
  `(c/let [closure# (zipmap (keys *dynamic-vars*)
                            (map #(doto (Var/create (deref %)) .setDynamic)
                                 (vals *dynamic-vars*)))]
          (def-helper* fn nil lambda ~(first cdr)
            (binding [*dynamic-vars* (merge *dynamic-vars* closure#)]
              ~@(rest cdr)))))

(c/defmacro unwind-protect
  "Do BODYFORM, protecting with UNWINDFORMS.
  If BODYFORM completes normally, its value is returned
  after executing the UNWINDFORMS.
  If BODYFORM exits nonlocally, the UNWINDFORMS are executed anyway."
  {:arglists '([BODYFORM UNWINDFORMS...])}
  [bodyform & unwindforms]
  `(try
     ~(el->clj bodyform)
     (finally (progn ~@unwindforms))))

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
  (c/let [var (if (= () var) nil var)]
         `(try
            ~(el->clj bodyform)
            (catch deuce.emacs_lisp.error e#
              (c/let [~(if var var (gensym "_")) (:data (.state e#))]
                     (case (:symbol (.state e#))
                       ~@(apply concat (for [[c & h] handlers]
                                         `[~(sym c) (progn ~@h)]))
                       (throw e#)))))))

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
            (map #(do [`(not-null? ~(el->clj (first %)))
                       (if (second %) `(progn ~@(rest %)) (el->clj (first %)))]))
            (apply concat))))

(c/defmacro setq-helper* [locals default? sym-vals]
  `(c/let
       ~(reduce into []
                (for [[s v] (partition 2 sym-vals)
                      :let [s (expand-symbol s)]]
                  [(sym s) `(el-var-set ~s ~(el->clj v))]))
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

(c/defmacro let-helper* [can-refer? varlist & body]
  (c/let [varlist (map #(if (symbol? %) [% nil] %) varlist)
          all-vars (map first varlist)
          temps (zipmap all-vars (repeatedly #(gensym "local__")))]
         `(c/let ~(vec (concat
                        (interleave (map (if can-refer? identity temps) (map first varlist))
                                    (map (comp el->clj second) varlist))
                        (when-not can-refer? (interleave all-vars (map temps all-vars)))))
                 (with-local-el-vars ~(interleave all-vars all-vars)
                   (progn ~@body)))))

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
  (c/let [symbol (expand-symbol symbol)]
    `(do
       (-> (intern (create-ns 'deuce.emacs-lisp.globals)
                   '~symbol
                   ~(el->clj initvalue))
           (alter-meta! merge {:doc ~(apply str docstring)}))
       '~symbol)))

(c/defmacro prog1
  "Eval FIRST and BODY sequentially; return value from FIRST.
  The value of FIRST is saved during the evaluation of the remaining args,
  whose values are discarded."
  {:arglists '([FIRST BODY...])}
  [first & body]
  `(c/let [result# ~(el->clj first)]
          (progn ~@body)
          result#))

(c/defmacro prog2
  "Eval FORM1, FORM2 and BODY sequentially; return value from FORM2.
  The value of FORM2 is saved during the evaluation of the
  remaining args, whose values are discarded."
  {:arglists '([FORM1 FORM2 BODY...])}
  [form1 form2 & body]
  `(progn ~form1
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
  `(c/or ~@(map #(do `(not-null? ~(el->clj %))) conditions)))

(c/defmacro while
  "If TEST yields non-nil, eval BODY... and repeat.
  The order of execution is thus TEST, BODY, TEST, BODY and so on
  until TEST returns nil."
  {:arglists '([TEST BODY...])}
  [test & body]
  `(c/while (not-null? ~(el->clj test)) (progn ~@body)))

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
  `(c/and ~@(map #(do `(not-null? ~(el->clj %))) conditions)))

(c/defmacro progn
  "Eval BODY forms sequentially and return value of last one."
  {:arglists '([BODY...])}
  [& body]
  `(do ~@(map el->clj body)))

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
  (c/let [symbol (expand-symbol symbol)
          ^Var default (global symbol)
          m (meta default)]
    (->
     ^Var (intern (create-ns ns)
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
  (c/let [emacs-lisp? (= (the-ns 'deuce.emacs) *ns*)]
         `(defvar-helper* 'deuce.emacs-lisp.globals '~(expand-symbol symbol)
            ~(if emacs-lisp? (el->clj initvalue) initvalue) ~docstring)))

;; defined as fn in eval.clj
(c/defmacro ^:clojure-special-form throw
  "Throw to the catch for TAG and return VALUE from it.
  Both TAG and VALUE are evalled."
  {:arglists '([TAG VALUE])}
  [tag value]
  `(throw (deuce.emacs_lisp.error. ~value ~tag)))

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
     (progn ~@body)
     (catch deuce.emacs_lisp.error e#
       (if (= ~tag (:symbol (.state e#)))
         (:data (.state e#))
         (throw e#)))))

(c/defmacro ^:clojure-special-form if
  "If COND yields non-nil, do THEN, else do ELSE...
  Returns the value of THEN or the value of the last of the ELSE's.
  THEN must be one expression, but ELSE... can be zero or more expressions.
  If COND yields nil, and there are no ELSE's, the value is nil."
  {:arglists '([COND THEN ELSE...])}
  [cond then & else]
  `(c/cond (not-null? ~(el->clj cond)) ~(el->clj then)
           :else (progn ~@else)))

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
  `(progn ~@body))

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
  `(progn ~@body))

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
  `(progn ~@body))

(def clojure-special-forms
  (->> (ns-map 'deuce.emacs-lisp)
       (filter (comp :clojure-special-form meta val))
       (into {})))

(defn check-type [pred x]
  (when-not ((fun pred) x)
    (deuce.emacs-lisp/throw 'wrong-type-argument (cons/list pred x))))

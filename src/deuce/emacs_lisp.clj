(ns deuce.emacs-lisp
  (:require [clojure.core :as c]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.walk :as w]
            [deuce.emacs-lisp.cons :as cons])
  (:use [taoensso.timbre :as timbre
         :only (trace debug info warn error fatal spy)])
  (:import [clojure.lang Var ExceptionInfo]
           [java.io Writer]
           [java.lang.reflect Method])
  (:refer-clojure :exclude [defmacro and or cond let while eval set compile]))

(timbre/set-config! [:prefix-fn]
                    (fn [{:keys [level timestamp hostname ns]}]
                      (str timestamp " " (-> level name s/upper-case) " [" ns "]")))
(timbre/set-config! [:timestamp-pattern] "HH:mm:ss,SSS")

(timbre/set-level! :warn)
(var-set  #'*warn-on-reflection* true)

(create-ns 'deuce.emacs)
(create-ns 'deuce.emacs-lisp.globals)

(declare clojure-special-forms throw defvar el->clj eval emacs-lisp-backquote)

(defn vector-reader [v]
  (object-array (vec v)))

(defn symbol-reader [s]
  (symbol nil s))

(defn sym [s]
  (symbol nil (name s)))

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

(defn emacs-lisp-error [tag value]
  (proxy [ExceptionInfo] [(str tag) {:tag tag :value value}]
    (getMessage [] (str this))
    (toString []
      (str (cons (:tag (.data ^ExceptionInfo this))
                 (c/let [d (:value (.-data ^ExceptionInfo this))]
                        (if (seq? d) d [d])))))))

(defn throw* [tag value]
  (throw (emacs-lisp-error tag value)))

(def ^:dynamic *dynamic-vars* {})

;; There're also buffer local variables, which will be using deuce.emacs.buffer/current-buffer
;; These vars are introduced by deuce.emacs.data/make-local-variable or make-variable-buffer-local
;; There's also an obsolete (as of Emacs 22.2) concept of frame locals.
;; See deuce.emacs.data/make-variable-frame-local and deuce.emacs.frame/modify-frame-parameters
(defn el-var-buffer-local [name])

(defn el-var [name]
  ((some-fn *dynamic-vars* el-var-buffer-local global) name))

(defn el-var-get* [name]
  (c/let [name (sym name)]
         (if-let [v (el-var name)]
           @v
           (throw* 'void-variable name))))

(c/defmacro el-var-get [name]
  (c/let [name (sym name)]
         (if (c/and (symbol? name) (name &env))
           `(if (var? ~name) @~name ~name)
           `(el-var-get* '~name))))

;; split these out into el-var-set-** and get rid of eval in deuce.emacs.data/set(-default)
(c/defmacro el-var-set-default [name value]
  (c/let [name (sym name)]
         `(c/let [value# ~value]
                 (if-let [v# (global '~name)]
                   (alter-var-root v# (constantly ~value))
                   @(global (defvar ~name ~value))))))

(c/defmacro el-var-set [name value]
  (c/let [name (sym name)]
         `(c/let [value# ~value]
                 (if-let [^Var v# (c/or ~(c/and (symbol? name) (name &env) name)
                                        ((some-fn *dynamic-vars* el-var-buffer-local) '~name))]
                   (if (c/and (.hasRoot v#) (not (.getThreadBinding v#)))
                     (alter-var-root v# (constantly value#))
                     (var-set v# value#))
                   (el-var-set-default ~name value#)))))

(defn dynamic-binding? []
  (not (el-var-get lexical-binding)))

(c/defmacro with-local-el-vars [name-vals-vec & body]
  (c/let [vars (vec (map sym (take-nth 2 name-vals-vec)))
          vals (vec (take-nth 2 (rest name-vals-vec)))]
         `(c/let [vars# (hash-map ~@(interleave (map #(list 'quote %) vars)
                                                (map #(do `(c/or (*dynamic-vars* '~%) (global '~%)
                                                                 (c/doto (Var/create) .setDynamic))) vars)))]
                 (with-bindings (zipmap (map vars# '~vars) ~vals)
                   (binding [*dynamic-vars* (if (dynamic-binding?) (merge *dynamic-vars* vars#) {})]
                     (c/let [{:syms ~vars} vars#]
                            ~@body))))))

(def ^:dynamic *disallow-undefined* #{})

;; build cached invoker to use once target is resolved?
(defn delayed-eval* [expr]
  (binding [*disallow-undefined* (conj *disallow-undefined* (first expr))]
    (eval expr)))

(c/defmacro delayed-eval [expr]
  `(delayed-eval* '~expr))

(defn expand-dotted-lists [x]
  (if (c/or (cons/dotted-list? x) (cons/dotted-pair? x))
     (apply cons/list x)
     x))

(defn el->clj [x]
  (condp some [x]
    #{()} nil
    seq? (c/let [[fst & rst] x]
                (if (c/and (symbol? fst)
                           (not= 'progn fst)
                           (-> (fun fst) meta :macro))
                  (if (c/or (clojure-special-forms fst) (= 'lambda fst))
                    (if (= 'quote fst)
                      (if-let [s (c/and (symbol? (first rst)) (not (next rst)) (first rst))]
                        (list 'quote (if (= "deuce.emacs" (namespace s)) (sym s) s))
                        (if (= '(()) rst) () x))
                      (c/cons (symbol "deuce.emacs-lisp" (name fst)) rst))
                    x)
                  (if (`#{el-var-get el-var-set el-var-set-default} fst)
                    x
                    (if (=  '#el/sym "\\`" fst)
                      (emacs-lisp-backquote x) ;; See below, we dont want to duplicate this if not necessary.
                      (if (c/and (symbol? fst)
                                 (not (namespace fst))
                                 (not (fun fst)))
                        (if (*disallow-undefined* fst)
                          `(throw* '~'void-function '~fst)
                          (list `delayed-eval x))

                        (expand-dotted-lists (c/cons
                                              (if (seq? fst) (el->clj fst) fst)
                                              (map el->clj rst))))))))
    symbol? (if (namespace x)
              (if (-> (resolve x) meta :macro) (resolve x) x)
              (list `el-var-get x))
    x))

(defn ^Throwable cause [^Throwable e]
  (if-let [e (.getCause e)]
    (recur e)
    e))

(def ^Method clojure-syntax-quote
  (doto
      (.getDeclaredMethod clojure.lang.LispReader$SyntaxQuoteReader
                          "syntaxQuote"
                          (into-array [Object]))
    (.setAccessible true)))

(defn syntax-quote* [form]
  (.invoke clojure-syntax-quote nil (into-array [form])))

;; There's a version of this already defined as macro in backquote.el, use it / override it?
;; What's their relationship?
(defn emacs-lisp-backquote [form]
  (w/postwalk
   #(c/cond
     (c/and (seq? %) (= '#el/sym "\\`" (first %)))
     (w/postwalk cons/maybe-seq (el->clj (syntax-quote* (second %))))
     (= '#el/sym "\\," %) `unquote
     (= '#el/sym "\\,@" %) `unquote-splicing
     :else %) form))

;; Explore to either get rid of or just using the macro, not both el->clj and it
;; (c/defmacro #el/sym "\\`" [form]
;;   (emacs-lisp-backquote (list '#el/sym "\\`" form)))

(defn compile [emacs-lisp]
  (try
    (when emacs-lisp (c/eval (if (meta emacs-lisp) (with-meta emacs-lisp nil) emacs-lisp)))
    (catch RuntimeException e
      (do
        (error (-> e cause .getMessage) (with-out-str (pp/pprint emacs-lisp)))
        (throw e)))))

;; defined in eval.clj
(defn eval [body & [lexical]]
  (binding [*ns* (the-ns 'deuce.emacs)]
    (with-bindings (if lexical {(global 'lexical-binding) true} {})
      (maybe-sym (compile (el->clj body))))))

(defn parse-doc-string [[doc & rst :as body]]
  (if (string? doc)
    [doc rst]
    [nil body]))

(c/defmacro def-helper* [what line name arglist & body]
  (c/let [[docstring body] (parse-doc-string body)
          name (sym name)
          el-arglist arglist
          rest-arg (maybe-sym (second (drop-while (complement '#{&rest}) arglist)))
          [arg & args :as arglist] (map sym (replace '{&rest &} arglist))
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
          the-args (remove '#{&} (flatten arglist))
          needs-intern? (when (c/and (re-find #"/" (c/name name)) (not= '/ name)) (gensym))]
         `(c/let [f# (~what ~(if needs-intern? needs-intern? name) ~(vec arglist)
                            ~(when-not (seq body)
                               `(binding [*ns* (the-ns 'clojure.core)]
                                  (warn ~(c/name name) "NOT IMPLEMENTED")))
                            ~(if emacs-lisp?
                               `(c/let ~(if rest-arg
                                          `[~rest-arg (if-let [r# ~rest-arg] (apply cons/list r#) nil)]
                                          [])
                                       (c/let [result# (with-local-el-vars ~(vec (mapcat #(c/list % %) the-args))
                                                         (progn ~@body))]
                                              ;; There's something wrong with the returned forms, hence the prewalk
                                              (if ~macro?
                                                (w/prewalk identity (el->clj result#))
                                                result#)))
                               `(do ~@body)))]
                 (if (var? f#)
                   (do
                     (alter-meta! f# merge (merge {:doc ~doc
                                                   :el-arglist '~(seq el-arglist)}
                                                  (when ~emacs-lisp?
                                                    {:el-file (when-let [file# (el-var 'load-file-name)]
                                                                @file#)})))
                     (alter-var-root f# (constantly (with-meta @f# (meta f#))))
                     (when ~needs-intern?
                       (intern 'deuce.emacs (with-meta '~name (dissoc (meta f#) :name)) @f#)
                       (ns-unmap 'deuce.emacs '~needs-intern?)))
                   (with-meta f# (assoc (meta f#) :doc ~doc))))))

(def override? '#{apply-partially})

(c/defmacro defun
  "Define NAME as a function.
  The definition is (lambda ARGLIST [DOCSTRING] BODY...).
  See also the function `interactive'."
  {:arglists '([NAME ARGLIST [DOCSTRING] BODY...])}
  [name arglist & body]
  (c/let [name (sym name)]
         `(do ~(when-not (override? name)
                 `(def-helper* defn ~(-> name meta :line) ~name ~arglist ~@body))
              '~name)))

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
  (c/let [[args & body] cdr
          [docstring body] (parse-doc-string body)
          doc (apply str docstring)
          vars (remove #(re-find #"__\d+" (name %)) (keys &env))
          vars (vec (remove (c/set args) vars))]
         ;; This is wrong as it won't share updates between original definition and the lambda var.
         ;; Yet to see if this ends up being a real issue.
         `(c/let [closure# (zipmap '~vars
                                   (map #(doto (Var/create (if (var? %) (deref %) %)) .setDynamic)
                                        ~vars))]
                 (with-meta
                   (def-helper* fn nil lambda ~args
                     (binding [*dynamic-vars* (if (dynamic-binding?) (merge *dynamic-vars* closure#) {})]
                       (c/let [{:syms ~vars} closure#]
                              (progn ~@body))))
                   {:doc ~doc}))))

;; defined in subr.el
(c/defmacro declare-function
 "Tell the byte-compiler that function FN is defined, in FILE.
  Optional ARGLIST is the argument list used by the function.  The
  FILE argument is not used by the byte-compiler, but by the
  `check-declare' package, which checks that FILE contains a
  definition for FN.  ARGLIST is used by both the byte-compiler and
  `check-declare' to check for consistency.

  FILE can be either a Lisp file (in which case the \".el\"
  extension is optional), or a C file.  C files are expanded
  relative to the Emacs \"src/\" directory.  Lisp files are
  searched for using `locate-library', and if that fails they are
  expanded relative to the location of the file containing the
  declaration.  A FILE with an \"ext:\" prefix is an external file.
  `check-declare' will check such files if they are found, and skip
  them without error if they are not.

  FILEONLY non-nil means that `check-declare' will only check that
  FILE exists, not that it defines FN.  This is intended for
  function-definitions that `check-declare' does not recognize, e.g.
  `defstruct'.

  To specify a value for FILEONLY without passing an argument list,
  set ARGLIST to t.  This is necessary because nil means an
  empty argument list, rather than an unspecified one.

  Note that for the purposes of `check-declare', this statement
  must be the first non-whitespace on a line.

  For more information, see Info node `(elisp)Declaring Functions'."
 [fn file & [arglist fileonly]]
 {:arglists '([FN FILE &optional ARGLIST FILEONLY])}
 (c/let [name (sym fn)]
        `(do
           ~(when-not (fun name)
              `(do (def-helper* defn nil ~name ~arglist nil)
                   (alter-meta! (el/fun '~name) merge {:file '~file :declared true})))
           '~name)))

;; defined in subr.el
(defn apply-partially
  "Return a function that is a partial application of FUN to ARGS.
  ARGS is a list of the first N arguments to pass to FUN.
  The result is a new function which does the same as FUN, except that
  the first N arguments are fixed at the values with which this function
  was called."
  [fun & args]
  (fn partial [& new-args]
    (apply (deuce.emacs-lisp/fun fun) (concat args new-args))))

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
            (catch ExceptionInfo e#
              (c/let [~(if var var (gensym "_")) (:value (ex-data e#))]
                     (case (:tag (ex-data e#))
                       ~@(apply concat (for [[c & h] handlers
                                             :let [c (if (seq? c) c [c])]]
                                         (apply concat (for [c c] `[~(sym c) (progn ~@h)]))))
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

(c/defmacro setq-helper* [default? sym-vals]
  (c/let [emacs-lisp? (= (the-ns 'deuce.emacs) *ns*)]
         `(c/let
           ~(reduce into []
                    (for [[s v] (partition 2 sym-vals)
                          :let [s (sym s)]]
                      [(sym s) (if default?
                                 `(el-var-set-default ~s ~(if emacs-lisp? (el->clj v) v))
                                 `(el-var-set ~s ~(if emacs-lisp? (el->clj v) v)))]))
           ~(last (butlast sym-vals)))))

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
  `(setq-helper* false ~sym-vals))

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
          all-vars (map (comp sym first) varlist)
          temps (zipmap all-vars (repeatedly #(gensym "local__")))]
         `(c/let ~(vec (concat
                        (interleave (map (if can-refer? identity temps) all-vars)
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
  (c/let [symbol (sym symbol)]
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
  `(setq-helper* true ~var-values))

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
  (c/let [name (sym name)]
         `(do
            ~(when-not ((ns-interns 'deuce.emacs-lisp) name)
               `(def-helper* c/defmacro ~(-> name meta :line) ~name ~arglist ~@body))
            '~name)))

(c/defmacro function
  "Like `quote', but preferred for objects which are functions.
  In byte compilation, `function' causes its argument to be compiled.
  `quote' cannot do that."
  {:arglists '([ARG])}
  [arg]
  (if (c/and (seq? arg) (symbol? (first arg)) (= 'lambda (sym (first arg))))
    arg
    `(quote ~arg)))

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
  `(let-helper* true ~varlist ~@body))

(defn defvar-helper* [ns symbol & [initvalue docstring]]
  (c/let [symbol (sym symbol)
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
         `(defvar-helper* 'deuce.emacs-lisp.globals '~(sym symbol)
            ~(if emacs-lisp? (el->clj initvalue) initvalue) ~docstring)))

;; defined as fn in eval.clj
(c/defmacro ^:clojure-special-form throw
  "Throw to the catch for TAG and return VALUE from it.
  Both TAG and VALUE are evalled."
  {:arglists '([TAG VALUE])}
  [tag value]
  `(throw* ~tag ~value))

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
     (catch ExceptionInfo e#
       (if (= ~(el->clj tag) (:tag (ex-data e#)))
         (:value (ex-data e#))
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

;; Navgeet's helper macro, will revisit, basically condition-case but for use from Clojure
(c/defmacro try-with-tag [& exprs]
  (c/let [catch-clauses (c/filter #(c/= (first %) 'catch) exprs)
          finally-clause (c/filter #(c/= (first %) 'finally) exprs)
          try-exprs (c/remove #(c/or (c/= (first %) 'finally) (c/= (first %) 'catch)) exprs)]
         `(try ~@try-exprs
               ~@(for [expr catch-clauses]
                   (c/let [[_ tag e & exprs] expr]
                          `(catch ExceptionInfo e#
                             (if (= ~tag (:tag (ex-data e#)))
                               (c/let [~e e#]
                                      (do ~@exprs))
                               (throw e#)))))
               ~@finally-clause)))
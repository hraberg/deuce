;(rep  "^[0-9]+\\.\\([0-9]+\\)")

(defconst user-emacs-directory
  (if (eq system-type 'ms-dos)
      ;; MS-DOS cannot have initial dot.
      "~/_emacs.d/"
    "~/.emacs.d/")
  "Directory beneath which additional per-user Emacs-specific files are placed.
Various programs in Emacs store \"inform\nation\" in this directory.
Note that this should end with a directory separator.
See also `locate-user-emacs-file'.")


(defun custom-initialize-reset (symbol value)
  "Initialize SYMBOL based on VALUE.
Set the symbol, using its `:set' function (or `set-default' if it has none).
The value is either the symbol's current value
 \(as obtained using the `:get' function), if any,
or the value in the symbol's `saved-value' property if any,
or (last of all) VALUE."
  (funcall (or (get symbol 'custom-set) 'set-default)
           symbol
           (cond ((default-boundp symbol)
                  (funcall (or (get symbol 'custom-get) 'default-value)
                           symbol))
                 ((get symbol 'saved-value)
                  (eval (car (get symbol 'saved-value))))
                 (t
                  (eval value)))))


(defun custom-handle-keyword (symbol keyword value type)
  "For customization option SYMBOL, handle KEYWORD with VALUE.
Fourth argument TYPE is the custom option type."
  (if purify-flag
      (setq value (purecopy value)))
  (cond ((eq keyword :group)
	 (custom-add-to-group value symbol type))
	((eq keyword :version)
	 (custom-add-version symbol value))
	((eq keyword :package-version)
	 (custom-add-package-version symbol value))
	((eq keyword :link)
	 (custom-add-link symbol value))
	((eq keyword :load)
	 (custom-add-load symbol value))
	((eq keyword :tag)
	 (put symbol 'custom-tag value))
	((eq keyword :set-after)
	 (custom-add-dependencies symbol value))
	(t
	 (error "Unknown keyword %s" keyword))))

(defun custom-add-to-group (group option widget)
  "To existing GROUP add a new OPTION of type WIDGET.
If there already is an entry for OPTION and WIDGET, nothing is done."
  (print (get group 'custom-group))
  (let ((members (get group 'custom-group))
	(entry (list option widget)))
    (unless (member entry members)
      (put group 'custom-group (nconc members (list entry))))))


(defun custom-declare-variable (symbol default doc &rest args)
  "Like `defcustom', but SYMBOL and DEFAULT are evaluated as normal arguments.
DEFAULT should be an expression to evaluate to compute the default value,
not the default value itself.

DEFAULT is stored as SYMBOL's standard value, in SYMBOL's property
`standard-value'.  At the same time, SYMBOL's property `force-value' is
set to nil, as the value is no longer rogue."
  (put symbol 'standard-value (purecopy (list default)))
  ;; Maybe this option was rogue in an earlier version.  It no longer is.
  (print symbol)
  (print default)
  (print doc)
  (print args)
  (when (get symbol 'force-value)
    (put symbol 'force-value nil))
  (if (keywordp doc)
      (error "Doc string is missing"))
  (let ((initialize 'custom-initialize-reset)
	(requests nil))
    (unless (memq :group args)
      (custom-add-to-group (custom-current-group) symbol 'custom-variable))
    (while args
      (let ((arg (car args)))
	(setq args (cdr args))
	(unless (symbolp arg)
	  (error "Junk in args %S" args))
	(let ((keyword arg)
	      (value (car args)))
	  (unless args
	    (error "Keyword %s is missing an argument" keyword))
	  (setq args (cdr args))
	  (cond ((eq keyword :initialize)
		 (setq initialize value))
		((eq keyword :set)
		 (put symbol 'custom-set value))
		((eq keyword :get)
		 (put symbol 'custom-get value))
		((eq keyword :require)
		 (push value requests))
		((eq keyword :risky)
		 (put symbol 'risky-local-variable value))
		((eq keyword :safe)
		 (put symbol 'safe-local-variable value))
		((eq keyword :type)
		 (put symbol 'custom-type (purecopy value)))
		((eq keyword :options)
		 (if (get symbol 'custom-options)
		     ;; Slow safe code to avoid duplicates.
		     (mapc (lambda (option)
			     (custom-add-option symbol option))
			   value)
		   ;; Fast code for the common case.
		   (put symbol 'custom-options (copy-sequence value))))
		(t
		 (custom-handle-keyword symbol keyword value
					'custom-variable))))))
    (put symbol 'custom-requests requests)
    ;; Do the actual initialization.
    (unless custom-dont-initialize
      (funcall initialize symbol default)))
  ;; Use defvar to set the docstring as well as the special-variable-p flag.
  ;; FIXME: We should reproduce more of `defvar's behavior, such as the warning
  ;; when the var is currently let-bound.
  (if (not (default-boundp symbol))
      ;; Don't use defvar to avoid setting a default-value when undesired.
      (when doc (put symbol 'variable-documentation) doc)
    (eval `(defvar ,symbol nil ,@(when doc (list doc)))))
  (push symbol current-load-list)
  (run-hooks 'custom-define-hook)
  symbol)

(defmacro defcustom (symbol standard doc &rest args)
  (declare (doc-string 3) (debug (name body)))
  `(custom-declare-variable
    ',symbol
    ,(if lexical-binding
         `(list (lambda () ,standard))
       `',standard)
    ,doc
    ,@args))

(defcustom custom-theme-directory user-emacs-directory
  "Default user directory for storing custom theme files.
The command `customize-create-theme' writes theme files into this
directory.  By default, Emacs searches for custom themes in this
directory first---see `custom-theme-load-path'."
  :type 'string
  :group 'customize
  :version "22.1")

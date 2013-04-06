(ns deuce.emacs
  (:require [clojure.core :as c]
            [deuce.emacs-lisp :as el]
            [deuce.emacs-lisp.globals :as globals])
  (:refer-clojure :only [])
  (:use [deuce.emacs-lisp :only [and apply-partially catch cond condition-case defconst define-compiler-macro defmacro
                                 defun defvar function if interactive lambda let let* or prog1 prog2 progn quote
                                 save-current-buffer save-excursion save-restriction setq setq-default
                                 unwind-protect while throw]]
        [deuce.emacs.alloc]
        [deuce.emacs.buffer]
        [deuce.emacs.bytecode]
        [deuce.emacs.callint]
        [deuce.emacs.callproc]
        [deuce.emacs.casefiddle]
        [deuce.emacs.casetab]
        [deuce.emacs.category]
        [deuce.emacs.ccl]
        [deuce.emacs.character]
        [deuce.emacs.charset]
        [deuce.emacs.chartab]
        [deuce.emacs.cmds]
        [deuce.emacs.coding]
        [deuce.emacs.composite]
        [deuce.emacs.data]
        [deuce.emacs.dired]
        [deuce.emacs.dispnew]
        [deuce.emacs.doc]
        [deuce.emacs.editfns]
        [deuce.emacs.emacs]
        [deuce.emacs.eval]
        [deuce.emacs.fileio]
        [deuce.emacs.filelock]
        [deuce.emacs.floatfns]
        [deuce.emacs.fns]
        [deuce.emacs.font]
        [deuce.emacs.frame]
        [deuce.emacs.indent]
        [deuce.emacs.insdel]
        [deuce.emacs.keyboard]
        [deuce.emacs.keymap]
        [deuce.emacs.lread]
        [deuce.emacs.macros]
        [deuce.emacs.marker]
        [deuce.emacs.menu]
        [deuce.emacs.minibuf]
        [deuce.emacs.print]
        [deuce.emacs.process]
        [deuce.emacs.search]
        [deuce.emacs.syntax]
        [deuce.emacs.term]
        [deuce.emacs.terminal]
        [deuce.emacs.textprop]
        [deuce.emacs.undo]
        [deuce.emacs.window]
        [deuce.emacs.xdisp]
        [deuce.emacs.xfaces]
        [deuce.emacs.xml]))

(setq t true)

;; Stubs for running without MULE:
;; These keymaps are referenced from menu-bar.
(setq mule-menu-keymap (make-sparse-keymap))
(setq describe-language-environment-map (make-sparse-keymap))
;; Used by startup/normal-top-level to set the locale, called with nil.
(defun set-locale-environment (&optional locale-name frame))
(setq current-language-environment "English")
;; Used by startup/fancy-about-text to find localized tutorial.
(defun get-language-info (lang-env key)
  (({"English" {'tutorial "TUTORIAL"}} lang-env {}) key))
;; Used by env.
(defun find-coding-systems-string (string))
;; These are used by the mode line
(setq current-input-method)
(defun coding-system-eol-type-mnemonic (coding-system)
  (symbol-value ({0 'eol-mnemonic-unix 1 'eol-mnemonic-dos 2 'eol-mnemonic-mac}
                 (coding-system-eol-type coding-system) 'eol-mnemonic-undecided)))

;; I'm the one and only Frame
(setq terminal-frame ((c/ns-resolve 'deuce.emacs.frame 'make-initial-frame)))
(setq last-event-frame terminal-frame)

;; Callback run by faces/tty-run-terminal-initialization based on deuce.emacs.term/tty-type returning "lanterna"
(defun terminal-init-lanterna ()
  (c/require 'deuce.main)
  ((c/ns-resolve 'deuce.main 'terminal-init-lanterna))
  ;; Initialize the real TERM, should setup input-decode-map and local-function-key-map
  (setq xterm-extra-capabilities nil) ;; Don't allow term/xterm to check by sending stuff to the tty.
  (tty-run-terminal-initialization (selected-frame) (getenv-internal "TERM")))

;; Create *Deuce* log buffer first so it won't get selected.
(get-buffer-create "*Deuce*")
;; *Messages* is created by xdisp.c
(get-buffer-create "*Messages*")
;; *scratch* is created by buffer.c
(set-window-buffer (selected-window)
                   (get-buffer-create "*scratch*"))
;; Minibuffer 0 is the empty one, this is either created by frame.c or minibuffer.c
;; Not the leading space for buffers in the minibuffer window. *Minibuf-1* etc. gets created once it gets activated.
;; You can switch to these buffers in a normal window in Emacs and see them change as they're used.
(set-window-buffer (minibuffer-window)
                   (get-buffer-create " *Minibuf-0*"))
;; ensure_echo_area_buffers in xdisp.c creates (at least) two echo areas.
(get-buffer-create " *Echo Area 0*")
(get-buffer-create " *Echo Area 1*")

;; Hack for a predicate in cl.el, this is defined in emacs-lisp/bytecomp.el, which we're not using
(defun byte-compile-file-form (form))
;; ;; AOT cl.el gets confused by this alias
(defalias 'cl-block-wrapper 'identity)
(defmacro declare (&rest _specs) nil)
;; with-no-warnings in byte-run.el needs this
(defun last (list &optional n))
;; subr defines a simpler dolist, which custom uses, which gets redefined by cl-macs.
;; During AOT custom loads the latter dolist definition, requiring 'block' - not yet defined.
;; cl cannot be loaded first, as it depends on help-fns, which depend on custom.
(defmacro block (name &rest body) (cons 'progn body))
;; Hack as delayed-eval doesn't (like some other things) work properly inside let-bindings.
;; Needs to be fixed properly, but let's see if we can get through the boot with this hack.
;; cl-setf-simple-store-p is used in  cl-macs/cl-setf-do-modify, delayed-eval call refers to earlier binding 'method'.
(defun cl-setf-simple-store-p (sym form))
;; Same issue in regexp-opt/regexp-opt. Calls this fn with earlier binding 'sorted-strings'
(defun regexp-opt-group (strings &optional paren lax))

;; Keymap setup, should in theory be in deuce.emacs.keymap, but cannot for a reason I forgot.
(setq global-map (make-keymap))
(use-global-map (symbol-value 'global-map))

;; These use internal-define-key in Emacs, which doesn't define the prefix as symbol, unlike define-prefix-command.
(setq esc-map (make-keymap))
(fset 'ESC-prefix (symbol-value 'esc-map))
(setq ctl-x-map (make-keymap))
(fset 'Control-X-prefix (symbol-value 'ctl-x-map))

;; Main prefix keymaps setup from keymap.c
(define-key globals/global-map "\\e" 'ESC-prefix)
(define-key globals/global-map "\\C-x" 'Control-X-prefix)

;; self-insert-command for standard keys setup in cmds.c
(define-key globals/global-map "\\C-i" 'self-insert-command)
(c/doseq [n (c/range 32 (c/inc 127))]
         (define-key globals/global-map (make-string 1 n) 'self-insert-command))
(c/doseq [n (c/range 160 (c/inc 256))]
         (define-key globals/global-map (make-string 1 n) 'self-insert-command))

;; buffer commands from buffer.c
(define-key globals/ctl-x-map "b" 'switch-to-buffer)
(define-key globals/ctl-x-map "k" 'kill-buffer)

;; case commands from casefiddle.c
(define-key globals/ctl-x-map "\\C-u" 'upcase-region)
(put 'upcase-region 'disabled true)
(define-key globals/ctl-x-map "\\C-l" 'downcase-region)
(put 'downcase-region 'disabled true)

(define-key globals/esc-map "u" 'upcase-word)
(define-key globals/esc-map "l" 'downcase-word)
(define-key globals/esc-map "c" 'capitalize-word)

;; basic movement commands setup in cmds.c
(define-key globals/global-map "\\C-a" 'beginning-of-line)
(define-key globals/global-map "\\C-b" 'backward-char)
(define-key globals/global-map "\\C-e" 'end-of-line)
(define-key globals/global-map "\\C-f" 'forward-char)

;; basic commands setup in keyboard.c
(define-key globals/global-map "\\C-z" 'suspend-emacs)
(define-key globals/ctl-x-map "\\C-z" 'suspend-emacs)
(define-key globals/esc-map "\\C-c" 'exit-recursive-edit)
(define-key globals/global-map "\\C-]" 'abort-recursive-edit)
(define-key globals/esc-map "x" 'execute-extended-command)
;; There's also a bunch of initial_define_lispy_key I skip here

;; scolling commands in window.c
(define-key globals/ctl-x-map, "<" 'scroll-left)
(define-key globals/ctl-x-map ">" 'scroll-right)

(define-key globals/global-map "\\C-v" 'scroll-up-command)
(define-key globals/esc-map "\\C-v" 'scroll-other-window)
(define-key globals/esc-map "v" 'scroll-down-command)

;; var is definied in keyboard.clj
(setq function-key-map (make-sparse-keymap))
;; This map has a few low-level (like delete-frame) key defs in keybaoard.c
(setq special-event-map (make-sparse-keymap))
(setq local-function-key-map (make-sparse-keymap))
(set-keymap-parent globals/local-function-key-map globals/function-key-map)

(setq input-decode-map (make-sparse-keymap))
(setq key-translation-map (make-sparse-keymap))

(setq minibuffer-local-map (make-sparse-keymap))
(setq minibuffer-local-ns-map (make-sparse-keymap))
(set-keymap-parent globals/minibuffer-local-ns-map globals/minibuffer-local-map)

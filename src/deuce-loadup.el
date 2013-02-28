;;; loadup.el --- load up standardly loaded Lisp files for Emacs

;; Copyright (C) 1985-1986, 1992, 1994, 2001-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is loaded into a bare Emacs to make a dumpable one.

;; If you add/remove Lisp files to be loaded here, consider the
;; following issues:

;; i) Any file loaded on any platform should appear in $lisp in src/lisp.mk.
;; Use the .el or .elc version as appropriate.

;; This ensures both that the Lisp files are compiled (if necessary)
;; before the emacs executable is dumped, and that they are passed to
;; make-docfile.  (Any that are not processed for DOC will not have
;; doc strings in the dumped Emacs.)  Because of this:

;; ii) If the file is loaded uncompiled, it should (where possible)
;; obey the doc-string conventions expected by make-docfile.

;;; Code:

;; Add subdirectories to the load-path for files that might get
;; autoloaded when bootstrapping.
(if (or (equal (nth 3 command-line-args) "bootstrap")
	(equal (nth 4 command-line-args) "bootstrap")
	(equal (nth 3 command-line-args) "unidata-gen.el")
	(equal (nth 4 command-line-args) "unidata-gen-files")
	;; In case CANNOT_DUMP.
	(string-match "src/bootstrap-emacs" (nth 0 command-line-args)))
    (let ((dir (car load-path)))
      ;; We'll probably overflow the pure space.
      (setq purify-flag nil)
      (setq load-path (list dir
			    (expand-file-name "emacs-lisp" dir)
			    (expand-file-name "language" dir)
			    (expand-file-name "international" dir)
			    (expand-file-name "textmodes" dir)))))

(if (eq t purify-flag)
    ;; Hash consing saved around 11% of pure space in my tests.
    (setq purify-flag (make-hash-table :test 'equal)))

(message "Using load-path %s" load-path)

(if (or (member (nth 3 command-line-args) '("dump" "bootstrap"))
	(member (nth 4 command-line-args) '("dump" "bootstrap")))
    ;; To reduce the size of dumped Emacs, we avoid making huge
    ;; char-tables.
    (setq inhibit-load-charset-map t))

;; We don't want to have any undo records in the dumped Emacs.
(set-buffer "*scratch*")
(setq buffer-undo-list t)

;; DEUCE: handles inlining, won't be used, but other parts references its macros.
(load "emacs-lisp/byte-run")
;; DEUCE: backquotes handling used by lread.c, not used in Deuce to avoid having the reader depending on Emacs Lisp.
;;        Instead I use the internal SyntaxQuoteReader from Clojure - may revisit.
(load "emacs-lisp/backquote")
;; DEUCE: Lisp helpers/setup, some things, like dolist etc, are replaced by cl.el
(load "subr")

;; Do it after subr, since both after-load-functions and add-hook are
;; implemented in subr.el.
(add-hook 'after-load-functions (lambda (f) (garbage-collect)))

;; We specify .el in case someone compiled version.el by mistake.
(load "version.el")

;; DEUCE: support for defining widgets as used by customize. Not used for hyperlinks etc, see button below.
;;        No real intention of supporting it, but custom assumes its there.
(load "widget")
;; DEUCE: custom subsystem, not strictly necessary, but other things depend on it being there.
;; (load "custom")
;; DEUCE: Yes/No prompt.
;; (load "emacs-lisp/map-ynp")
;; DEUCE: Adds custom support for built in variables.
;; (load "cus-start")
;; DEUCE: MULE defines and deals with character encodings, won't be used, but some fns might be needed.
;; (load "international/mule")
;; (load "international/mule-conf")
;; DEUCE: unix environment helpers, causes cl.el to be loaded.
;; (load "env")
;; DEUCE: support for loading files with different encodings, won't be used. Mapping to Java encodings might be needed.
;; (load "format")

;; DEUCE: all basic editor key bindings are setup here - many refer to fns loaded later on.
;; (load "bindings")
;; DEUCE: defines C-x 2, C-x o etc.
;; (load "window")  ; Needed here for `replace-buffer-in-windows'.
;; (setq load-source-file-function 'load-with-code-conversion)
;; DEUCE: defines C-x C-f, C-x C-s etc.
;; (load "files")

;; DEUCE: custom extensions for faces
;; (load "cus-face")
;; DEUCE: tty-run-terminal-initialization is defined here, uses TERM to load term/xterm.el (for example), we might add our own / sidestep.
;; (load "faces")  ; after here, `defface' may be used.

;; DEUCE: button provides hyperlinks even in keyboard mode, needed for the startup screen.
;; (load "button")
;; DEUCE: actual startup of Emacs, parses command lines, opens the first frame and displays welcome and *scratch*
;; (load "startup")

;; DEUCE: At this point normal-top-level will be available.
;;        Calling it should start Emacs and clojure-lanterna can be intialized.
;;        I want to drive out the boot backwards based on what's needed at this point - not mimic the C.
;;        Actual details of the init of different subsystems are a mix of C (hence .clj) and Emacs Lisp.
;;        See emacs.c for the lowlevel init, also: frame.c and window.c.
;;        The GNU Emacs buffer will be shown: "Welcome to GNU Emacs, one component of the GNU/Linux operating system."

;; DEUCE: large autoload loaddefs, not strictly necessary to just start Emacs.
;; (condition-case nil
;;     ;; Don't get confused if someone compiled this by mistake.
;;     (load "loaddefs.el")
;;   ;; In case loaddefs hasn't been generated yet.
;;   (file-error (load "ldefs-boot.el")))

;; DEUCE: minibuffer and simple (which defines fundamental mode) are argubly necessary to be "Emacs".
;; (load "minibuffer")
;; DEUCE: abbrev mode, references by simple below (to turn it off at times)
;; (load "abbrev")         ;lisp-mode.el and simple.el use define-abbrev-table.
;; DEUCE: large support file for Emacs, adds completion, paren matching, line movement and various things.
;; (load "simple")

;; DEUCE: the help system isn't critical, but a non-trivial interactive Emacs extension to get working.
;; (load "help")

;; DEUCE: We should now have Emacs running with only fundamental-mode available. Release 0.1.0.
;;        M-x butterfly is defined in misc.el, loaded via autoload, see loaddef above. It depends on play/animate.

;; DEUCE: About half-way through loadup.el here. Next up is languages (to skip), various search/replace and actual major modes.

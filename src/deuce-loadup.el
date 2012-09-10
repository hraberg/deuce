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

(load "emacs-lisp/byte-run")
(load "emacs-lisp/backquote")
(load "subr")

;; Do it after subr, since both after-load-functions and add-hook are
;; implemented in subr.el.
(add-hook 'after-load-functions (lambda (f) (garbage-collect)))

;; We specify .el in case someone compiled version.el by mistake.
(load "version.el")

(load "widget")
(load "custom")
(load "emacs-lisp/map-ynp")
(load "cus-start")
(load "international/mule")

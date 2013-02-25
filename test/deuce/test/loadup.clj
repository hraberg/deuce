(ns deuce.test.loadup
  (:use [deuce.test.common])
  (:import [clojure.lang Sequential]))

;; "E.1 Building Emacs"[1]

;; These are the first few lines of <deuce>/emacs/lisp/loadup.el as a test.
;; The state after loadup is dumped into Emacs itself during the build of GNU Emacs.

;; In Deuce, this may eventually happen (as an optimization), but this basically represents the Emacs boot.

;; [1] http://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Emacs.html

(with-fresh-emacs)

(repl first-lines-of-loadup-as-bootstrap-emacs
      ;; setup

      (setq command-line-args (list "src/bootstrap-emacs"))

      ;; copied verbatim from loadup.el

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
                                (expand-file-name "textmodes" dir))))) ⇒ Sequential
      (if (eq t purify-flag)
        ;; Hash consing saved around 11% of pure space in my tests.
        (setq purify-flag (make-hash-table :test 'equal)))             ⇒ nil

      (message "Using load-path %s" load-path)
      -| "Using load-path .*emacs-lisp.*language.*international.*textmodes"

      (length load-path)                                               ⇒ 5


      (if (or (member (nth 3 command-line-args) '("dump" "bootstrap"))
              (member (nth 4 command-line-args) '("dump" "bootstrap")))
        ;; To reduce the size of dumped Emacs, we avoid making huge
        ;; char-tables.
        (setq inhibit-load-charset-map t))

      ;; We don't want to have any undo records in the dumped Emacs.
      (set-buffer "*scratch*")
      (setq buffer-undo-list t)                                       ⇒ true

      (load "emacs-lisp/byte-run")                                    ⇒ true
      (load "emacs-lisp/backquote")                                   ⇒ true
      (load "subr")                                                   ⇒ true

      ;; Do it after subr, since both after-load-functions and add-hook are
      ;; implemented in subr.el.
      (add-hook 'after-load-functions (lambda (f) (garbage-collect)))

      ;; We specify .el in case someone compiled version.el by mistake.
      (load "version.el")                                             ⇒ true

      (emacs-version)                                                 ⇒ #"GNU Emacs 24.2 .jvm-.+clojure.+")
(ns deuce.emacs.insdel
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c])
  (:refer-clojure :exclude []))

(defvar combine-after-change-calls nil
  "Used internally by the `combine-after-change-calls' macro.")

(defvar check-markers-debug-flag nil
  "Non-nil means enable debugging checks for invalid marker positions.")

(defvar inhibit-modification-hooks nil
  "Non-nil means don't run any of the hooks that respond to buffer changes.
  This affects `before-change-functions' and `after-change-functions',
  as well as hooks attached to text properties and overlays.")

(defun combine-after-change-execute ()
  "This function is for use internally in `combine-after-change-calls'."
  )

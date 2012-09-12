(ns deuce.emacs.casetab
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c]
            [deuce.emacs.chartab :as chartab]
            [deuce.emacs.data :as data]
            [deuce.emacs.fns :as fns])
  (:refer-clojure :exclude []))

(fns/put 'case-table 'char-table-extra-slots 3)

(def ^:dynamic ^:private *standard-case-table* (atom (chartab/make-char-table 'case-table)))

(defun set-standard-case-table (table)
  "Select a new standard case table for new buffers.
  See `set-case-table' for more info on case tables."
  (reset! *standard-case-table* table))

(defun case-table-p (object)
  "Return t if OBJECT is a case table.
  See `set-case-table' for more information on these data structures."
  (and (data/char-table-p object) (= 'case-table (.purpose object))))

(defun current-case-table ()
  "Return the case table of the current buffer."
  )

(defun set-case-table (table)
  "Select a new case table for the current buffer.
  A case table is a char-table which maps characters
  to their lower-case equivalents.  It also has three \"extra\" slots
  which may be additional char-tables or nil.
  These slots are called UPCASE, CANONICALIZE and EQUIVALENCES.
  UPCASE maps each non-upper-case character to its upper-case equivalent.
   (The value in UPCASE for an upper-case character is never used.)
   If lower and upper case characters are in 1-1 correspondence,
   you may use nil and the upcase table will be deduced from DOWNCASE.
  CANONICALIZE maps each character to a canonical equivalent;
   any two characters that are related by case-conversion have the same
   canonical equivalent character; it may be nil, in which case it is
   deduced from DOWNCASE and UPCASE.
  EQUIVALENCES is a map that cyclically permutes each equivalence class
   (of characters with the same canonical equivalent); it may be nil,
   in which case it is deduced from CANONICALIZE."
  )

(defun standard-case-table ()
  "Return the standard case table.
  This is the one used for new buffers."
  @*standard-case-table*)

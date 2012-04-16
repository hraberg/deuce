(ns emacs.casetab (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun set-standard-case-table (table)
  "Select a new standard case table for new buffers.
  See `set-case-table' for more info on case tables.this-single-command-raw-keys is a built-in function in `C source
  code'."
  )

(defun case-table-p (object)
  "Return t if OBJECT is a case table."
  )

(defun current-case-table ()
  "Return the case table of the current buffer.find-coding-systems-region-internal is a built-in function in `C
  source code'."
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
  EQUIVALENCES is a map that cyclicly permutes each equivalence class
   (of characters with the same canonical equivalent); it may be nil,"
  )

(defun standard-case-table ()
  "Return the standard case table."
  )

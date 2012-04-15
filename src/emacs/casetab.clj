(ns emacs.casetab (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun set-standard-case-table (table)
  "Select a new standard case table for new buffers.\nSee `set-case-table' for more info on case tables.this-single-command-raw-keys is a built-in function in `C source\ncode'."
  )

(defun case-table-p (object)
  "Return t if OBJECT is a case table.\n"
  )

(defun current-case-table ()
  "Return the case table of the current buffer.find-coding-systems-region-internal is a built-in function in `C\nsource code'."
  )

(defun set-case-table (table)
  "Select a new case table for the current buffer.\nA case table is a char-table which maps characters\nto their lower-case equivalents.  It also has three \"extra\" slots\nwhich may be additional char-tables or nil.\nThese slots are called UPCASE, CANONICALIZE and EQUIVALENCES.\nUPCASE maps each non-upper-case character to its upper-case equivalent.\n (The value in UPCASE for an upper-case character is never used.)\n If lower and upper case characters are in 1-1 correspondence,\n you may use nil and the upcase table will be deduced from DOWNCASE.\nCANONICALIZE maps each character to a canonical equivalent;\n any two characters that are related by case-conversion have the same\n canonical equivalent character; it may be nil, in which case it is\n deduced from DOWNCASE and UPCASE.\nEQUIVALENCES is a map that cyclicly permutes each equivalence class\n (of characters with the same canonical equivalent); it may be nil,\n"
  )

(defun standard-case-table ()
  "Return the standard case table.\n"
  )

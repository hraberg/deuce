(ns
 deuce.emacs.undo
 (use [deuce.emacs-lisp :only (defun defvar)])
 (require [clojure.core :as c])
 (:refer-clojure :exclude []))

(defvar undo-strong-limit nil
  "Don't keep more than this much size of undo information.
  This limit is applied when garbage collection happens.
  When a previous command increases the total undo list size past this
  value, that command and the earlier commands that came before it are forgotten.
  However, the most recent buffer-modifying command's undo info
  is never discarded for this reason.
  
  The size is counted as the number of bytes occupied,
  which includes both saved text and other data.
  
  You can customize this variable.")

(defvar undo-limit nil
  "Keep no more undo information once it exceeds this size.
  This limit is applied when garbage collection happens.
  When a previous command increases the total undo list size past this
  value, the earlier commands that came before it are forgotten.
  
  The size is counted as the number of bytes occupied,
  which includes both saved text and other data.
  
  You can customize this variable.")

(defvar undo-outer-limit nil
  "Outer limit on size of undo information for one command.
  At garbage collection time, if the current command has produced
  more than this much undo information, it discards the info and displays
  a warning.  This is a last-ditch limit to prevent memory overflow.
  
  The size is counted as the number of bytes occupied, which includes
  both saved text and other data.  A value of nil means no limit.  In
  this case, accumulating one huge undo entry could make Emacs crash as
  a result of memory overflow.
  
  In fact, this calls the function which is the value of
  `undo-outer-limit-function' with one argument, the size.
  The text above describes the behavior of the function
  that variable usually specifies.
  
  You can customize this variable.")

(defvar undo-inhibit-record-point nil
  "Non-nil means do not record `point' in `buffer-undo-list'.")

(defvar undo-outer-limit-function nil
  "Function to call when an undo list exceeds `undo-outer-limit'.
  This function is called with one argument, the current undo list size
  for the most recent command (since the last undo boundary).
  If the function returns t, that means truncation has been fully handled.
  If it returns nil, the other forms of truncation are done.
  
  Garbage collection is inhibited around the call to this function,
  so it must make sure not to do a lot of consing.")

(defun undo-boundary ()
  "Mark a boundary between units of undo.
  An undo command will stop at this point,
  but another undo command will undo to the previous boundary."
  )

(defun primitive-undo (n list)
  "Undo N records from the front of the list LIST.
  Return what remains of the list."
  )

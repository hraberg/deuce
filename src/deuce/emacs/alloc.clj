(ns deuce.emacs.alloc
  (:use [deuce.emacs-lisp :only (defun defvar)]
        [taoensso.timbre :as timbre
         :only (trace debug info warn error fatal spy)])
  (:require [clojure.core :as c]
            [clojure.walk :as w]
            [deuce.emacs-lisp.cons :as cons])
  (:refer-clojure :exclude [vector cons list])
  (:import [java.util Arrays]))

(defvar purify-flag nil
  "Non-nil means loading Lisp code in order to dump an executable.
  This means that certain objects should be allocated in shared (pure) space.
  It can also be set to a hash-table, in which case this table is used to
  do hash-consing of the objects allocated to pure space.")

(defvar cons-cells-consed nil
  "Number of cons cells that have been consed so far.")

(defvar symbols-consed nil
  "Number of symbols that have been consed so far.")

(defvar post-gc-hook nil
  "Hook run after garbage collection has finished.")

(defvar gc-cons-percentage nil
  "*Portion of the heap used for allocation.
  Garbage collection can happen automatically once this portion of the heap
  has been allocated since the last garbage collection.
  If this portion is smaller than `gc-cons-threshold', this is ignored.")

(defvar gcs-done nil
  "Accumulated number of garbage collections done.")

(defvar gc-elapsed nil
  "Accumulated time elapsed in garbage collections.
  The time is in seconds as a floating point value.")

(defvar gc-cons-threshold nil
  "*Number of bytes of consing between garbage collections.
  Garbage collection can happen automatically once this many bytes have been
  allocated since the last garbage collection.  All data types count.

  Garbage collection happens automatically only when `eval' is called.

  By binding this temporarily to a large number, you can effectively
  prevent garbage collection during a part of the program.
  See also `gc-cons-percentage'.

  You can customize this variable.")

(defvar memory-signal-data nil
  "Precomputed `signal' argument for memory-full error.")

(defvar string-chars-consed nil
  "Number of string characters that have been consed so far.")

(defvar memory-full nil
  "Non-nil means Emacs cannot get much more Lisp memory.")

(defvar vector-cells-consed nil
  "Number of vector cells that have been consed so far.")

(defvar misc-objects-consed nil
  "Number of miscellaneous objects that have been consed so far.
  These include markers and overlays, plus certain objects not visible
  to users.")

(defvar garbage-collection-messages nil
  "Non-nil means display messages at start and end of garbage collection.

  You can customize this variable.")

(defvar pure-bytes-used nil
  "Number of bytes of shareable Lisp data allocated so far.")

(defvar intervals-consed nil
  "Number of intervals that have been consed so far.")

(defvar strings-consed nil
  "Number of strings that have been consed so far.")

(defvar floats-consed nil
  "Number of floats that have been consed so far.")

(defun make-bool-vector (length init)
  "Return a new bool-vector of length LENGTH, using INIT for each element.
  LENGTH must be a number.  INIT matters only in whether it is t or nil."
  )

(defun make-byte-code (arglist byte-code constants depth &optional docstring interactive-spec &rest elements)
  "Create a byte-code object with specified arguments as elements.
  The arguments should be the ARGLIST, bytecode-string BYTE-CODE, constant
  vector CONSTANTS, maximum stack size DEPTH, (optional) DOCSTRING,
  and (optional) INTERACTIVE-SPEC.
  The first four arguments are required; at most six have any
  significance.
  The ARGLIST can be either like the one of `lambda', in which case the arguments
  will be dynamically bound before executing the byte code, or it can be an
  integer of the form NNNNNNNRMMMMMMM where the 7bit MMMMMMM specifies the
  minimum number of arguments, the 7-bit NNNNNNN specifies the maximum number
  of arguments (ignoring &rest) and the R bit specifies whether there is a &rest
  argument to catch the left-over arguments.  If such an integer is used, the
  arguments will not be dynamically bound but will be instead pushed on the
  stack before executing the byte-code."
  )

(defun memory-use-counts ()
  "Return a list of counters that measure how much consing there has been.
  Each of these counters increments for a certain kind of object.
  The counters wrap around from the largest positive integer to zero.
  Garbage collection does not decrease them.
  The elements of the value are as follows:
    (CONSES FLOATS VECTOR-CELLS SYMBOLS STRING-CHARS MISCS INTERVALS STRINGS)
  All are in units of 1 = one object consed
  except for VECTOR-CELLS and STRING-CHARS, which count the total length of
  objects consed.
  MISCS include overlays, markers, and some internal types.
  Frames, windows, buffers, and subprocesses count as vectors
    (but the contents of a buffer's text do not count here)."
  )

(defun vector (&rest objects)
  "Return a newly created vector with specified arguments as elements.
  Any number of arguments, even zero arguments, are allowed."
  (object-array objects))

(defun string (&rest characters)
  "Concatenate all the argument characters and make the result a string."
  ;; Guard against interning as we allow modifications of String.value for now.
  (apply str characters))

(defun make-marker ()
  "Return a newly allocated marker which does not point at any place."
  )

(declare list)

(defun garbage-collect ()
  "Reclaim storage for Lisp objects no longer needed.
  Garbage collection happens automatically if you cons more than
  `gc-cons-threshold' bytes of Lisp data since previous garbage collection.
  `garbage-collect' normally returns a list with info on amount of space in use:
   ((USED-CONSES . FREE-CONSES) (USED-SYMS . FREE-SYMS)
    (USED-MISCS . FREE-MISCS) USED-STRING-CHARS USED-VECTOR-SLOTS
    (USED-FLOATS . FREE-FLOATS) (USED-INTERVALS . FREE-INTERVALS)
    (USED-STRINGS . FREE-STRINGS))
  However, if there was overflow in pure space, `garbage-collect'
  returns nil, because real GC can't be done.
  See Info node `(elisp)Garbage Collection'."
  (System/gc)
  '(()))

(defun cons (car cdr)
  "Create a new cons, give it CAR and CDR as components, and return it."
  (cons/pair (cons/maybe-seq car) (cons/maybe-seq cdr)))

(defun #el/sym "/=" (num1 num2)
  "Return t if first arg is not equal to second arg.  Both must be numbers or markers."
  (not (== num1 num2)))

(defun make-symbol (name)
  "Return a newly allocated uninterned symbol whose name is NAME.
  Its value and function definition are void, and its property list is nil."
  (symbol name))

(defun purecopy (obj)
  "Make a copy of object OBJ in pure storage.
  Recursively copies contents of vectors and cons cells.
  Does not copy symbols.  Copies strings without text properties."
  (cons/maybe-seq obj))

(defun memory-limit ()
  "Return the address of the last byte Emacs has allocated, divided by 1024.
  This may be helpful in debugging Emacs's memory usage.
  We divide the value by 1024 to make sure it fits in a Lisp integer."
  )

(defun make-vector (length init)
  "Return a newly created vector of length LENGTH, with each element being INIT.
  See also the function `vector'."
  (let [^objects vector (object-array length)]
    (Arrays/fill vector init)
    vector))

(defun make-string (length init)
  "Return a newly created string of length LENGTH, with INIT in each element.
  LENGTH must be an integer.
  INIT must be an integer that represents a character."
  (apply str (repeat length init)))

(defun make-list (length init)
  "Return a newly created list of length LENGTH, with each element being INIT."
  (apply list (repeat length init)))

(defun list (&rest objects)
  "Return a newly created list with specified arguments as elements.
  Any number of arguments, even zero arguments, are allowed."
  (apply cons/list objects))

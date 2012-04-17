(ns emacs.alloc (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun make-bool-vector (length init)
  "Return a new bool-vector of length LENGTH, using INIT for each element.
  LENGTH must be a number.  INIT matters only in whether it is t or nil."
  )

(defun make-byte-code (arglist byte-code constants depth &optional docstring interactive-spec &rest elements)
  "Create a byte-code object with specified arguments as elements.
  The arguments should be the arglist, bytecode-string, constant vector,
  stack size, (optional) doc string, and (optional) interactive spec.
  The first four arguments are required; at most six have any
  significance."
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
  )

(defun string (&rest characters)
  "Concatenate all the argument characters and make the result a string."
  )

(defun make-marker ()
  "Return a newly allocated marker which does not point at any place."
  )

(defun garbage-collect ()
  "Reclaim storage for Lisp objects no longer needed.
  Garbage collection happens automatically if you cons more than
  `gc-cons-threshold' bytes of Lisp data since previous garbage collection.
  `garbage-collect' normally returns a list with info on amount of space in use:
   ((USED-CONSES . FREE-CONSES) (USED-SYMS . FREE-SYMS)
    (USED-MARKERS . FREE-MARKERS) USED-STRING-CHARS USED-VECTOR-SLOTS
    (USED-FLOATS . FREE-FLOATS) (USED-INTERVALS . FREE-INTERVALS)
    (USED-STRINGS . FREE-STRINGS))
  However, if there was overflow in pure space, `garbage-collect'
  returns nil, because real GC can't be done."
  )

(defun cons (car cdr)
  "Create a new cons, give it CAR and CDR as components, and return it."
  )

(defun make-symbol (name)
  "Return a newly allocated uninterned symbol whose name is NAME.
  Its value and function definition are void, and its property list is nil."
  )

(defun purecopy (obj)
  "Make a copy of object OBJ in pure storage.
  Recursively copies contents of vectors and cons cells.
  Does not copy symbols.  Copies strings without text properties."
  )

(defun memory-limit ()
  "Return the address of the last byte Emacs has allocated, divided by 1024.
  This may be helpful in debugging Emacs's memory usage.
  We divide the value by 1024 to make sure it fits in a Lisp integer."
  )

(defun make-vector (length init)
  "Return a newly created vector of length LENGTH, with each element being INIT.
  See also the function `vector'."
  )

(defun make-string (length init)
  "Return a newly created string of length LENGTH, with INIT in each element.
  LENGTH must be an integer.
  INIT must be an integer that represents a character."
  )

(defun make-list (length init)
  "Return a newly created list of length LENGTH, with each element being INIT."
  )

(defun list (&rest objects)
  "Return a newly created list with specified arguments as elements.
  Any number of arguments, even zero arguments, are allowed."
  )

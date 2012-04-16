(ns emacs.alloc (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun make-bool-vector (length init)
  "Return a new bool-vector of length LENGTH, using INIT for each element."
  )

(defun make-byte-code (arglist byte-code constants depth &optional docstring interactive-spec &rest elements)
  "Create a byte-code object with specified arguments as elements.
  The arguments should be the arglist, bytecode-string, constant vector,
  stack size, (optional) doc string, and (optional) interactive spec.
  The first four arguments are required; at most six have any"
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
  Frames, windows, buffers, and subprocesses count as vectors"
  )

(defun vector (&rest objects)
  "Return a newly created vector with specified arguments as elements."
  )

(defun string (&rest characters)
  )

(defun make-marker ()
  )

(defun cons (car cdr)
  )

(defun make-symbol (name)
  "Return a newly allocated uninterned symbol whose name is NAME.
  Its value and function definition are void, and its property list is nil.make-frame-invisible is an interactive built-in function in `C source
  code'."
  )

(defun purecopy (obj)
  "Make a copy of object OBJ in pure storage.
  Recursively copies contents of vectors and cons cells."
  )

(defun memory-limit ()
  "Return the address of the last byte Emacs has allocated, divided by 1024.
  This may be helpful in debugging Emacs's memory usage."
  )

(defun make-vector (length init)
  "Return a newly created vector of length LENGTH, with each element being INIT."
  )

(defun make-string (length init)
  "Return a newly created string of length LENGTH, with INIT in each element.
  LENGTH must be an integer."
  )

(defun make-list (length init)
  )

(defun list (&rest objects)
  "Return a newly created list with specified arguments as elements."
  )

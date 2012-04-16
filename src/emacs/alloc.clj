(ns emacs.alloc (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun make-bool-vector (length init)
  "Return a new bool-vector of length LENGTH, using INIT for each element.\n"
  )

(defun make-byte-code (arglist byte-code constants depth &optional docstring interactive-spec &rest elements)
  "Create a byte-code object with specified arguments as elements.\nThe arguments should be the arglist, bytecode-string, constant vector,\nstack size, (optional) doc string, and (optional) interactive spec.\nThe first four arguments are required; at most six have any\n"
  )

(defun memory-use-counts ()
  "Return a list of counters that measure how much consing there has been.\nEach of these counters increments for a certain kind of object.\nThe counters wrap around from the largest positive integer to zero.\nGarbage collection does not decrease them.\nThe elements of the value are as follows:\n  (CONSES FLOATS VECTOR-CELLS SYMBOLS STRING-CHARS MISCS INTERVALS STRINGS)\nAll are in units of 1 = one object consed\nexcept for VECTOR-CELLS and STRING-CHARS, which count the total length of\nobjects consed.\nMISCS include overlays, markers, and some internal types.\nFrames, windows, buffers, and subprocesses count as vectors\n"
  )

(defun vector (&rest objects)
  "Return a newly created vector with specified arguments as elements.\n"
  )

(defun string (&rest characters)
  )

(defun make-marker ()
  )

(defun cons (car cdr)
  )

(defun make-symbol (name)
  "Return a newly allocated uninterned symbol whose name is NAME.\nIts value and function definition are void, and its property list is nil.make-frame-invisible is an interactive built-in function in `C source\ncode'."
  )

(defun purecopy (obj)
  "Make a copy of object OBJ in pure storage.\nRecursively copies contents of vectors and cons cells.\n"
  )

(defun memory-limit ()
  "Return the address of the last byte Emacs has allocated, divided by 1024.\nThis may be helpful in debugging Emacs's memory usage.\n"
  )

(defun make-vector (length init)
  "Return a newly created vector of length LENGTH, with each element being INIT.\n"
  )

(defun make-string (length init)
  "Return a newly created string of length LENGTH, with INIT in each element.\nLENGTH must be an integer.\n"
  )

(defun make-list (length init)
  )

(defun list (&rest objects)
  "Return a newly created list with specified arguments as elements.\n"
  )

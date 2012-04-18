(ns
 deuce.emacs.floatfns
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude [float]))

(defun ftruncate (arg)
  "Truncate a floating point number to an integral float value.
  Rounds the value toward zero."
  )

(defun tan (arg)
  "Return the tangent of ARG."
  )

(defun atan (y &optional x)
  "Return the inverse tangent of the arguments.
  If only one argument Y is given, return the inverse tangent of Y.
  If two arguments Y and X are given, return the inverse tangent of Y
  divided by X, i.e. the angle in radians between the vector (X, Y)
  and the x-axis."
  )

(defun expt (arg1 arg2)
  "Return the exponential ARG1 ** ARG2."
  )

(defun sqrt (arg)
  "Return the square root of ARG."
  )

(defun abs (arg)
  "Return the absolute value of ARG."
  )

(defun fceiling (arg)
  "Return the smallest integer no less than ARG, as a float.
  (Round toward +inf.)"
  )

(defun float (arg)
  "Return the floating point number equal to ARG."
  )

(defun log (arg &optional base)
  "Return the natural logarithm of ARG.
  If the optional argument BASE is given, return log ARG using that base."
  )

(defun floor (arg &optional divisor)
  "Return the largest integer no greater than ARG.
  This rounds the value towards -inf.
  With optional DIVISOR, return the largest integer no greater than ARG/DIVISOR."
  )

(defun round (arg &optional divisor)
  "Return the nearest integer to ARG.
  With optional DIVISOR, return the nearest integer to ARG/DIVISOR."
  )

(defun log10 (arg)
  "Return the logarithm base 10 of ARG."
  )

(defun ffloor (arg)
  "Return the largest integer no greater than ARG, as a float.
  (Round towards -inf.)"
  )

(defun truncate (arg &optional divisor)
  "Truncate a floating point number to an int.
  Rounds ARG toward zero.
  With optional DIVISOR, truncate ARG/DIVISOR."
  )

(defun ceiling (arg &optional divisor)
  "Return the smallest integer no less than ARG.
  This rounds the value towards +inf.
  With optional DIVISOR, return the smallest integer no less than ARG/DIVISOR."
  )

(defun sin (arg)
  "Return the sine of ARG."
  )

(defun asin (arg)
  "Return the inverse sine of ARG."
  )

(defun fround (arg)
  "Return the nearest integer to ARG, as a float."
  )

(defun acos (arg)
  "Return the inverse cosine of ARG."
  )

(defun exp (arg)
  "Return the exponential base e of ARG."
  )

(defun cos (arg)
  "Return the cosine of ARG."
  )

(defun logb (arg)
  "Returns largest integer <= the base 2 log of the magnitude of ARG.
  This is the same as the exponent of a float."
  )

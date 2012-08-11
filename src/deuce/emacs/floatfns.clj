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

(defun ldexp (sgnfcand &optional exponent)
  "Construct number X from significand SGNFCAND and exponent EXP.
  Returns the floating point value resulting from multiplying SGNFCAND
  (the significand) by 2 raised to the power of EXP (the exponent)."
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
  With optional DIVISOR, return the nearest integer to ARG/DIVISOR.
  
  Rounding a value equidistant between two integers may choose the
  integer closer to zero, or it may prefer an even integer, depending on
  your machine.  For example, (round 2.5) can return 3 on some
  systems, but 2 on others."
  )

(defun log10 (arg)
  "Return the logarithm base 10 of ARG."
  )

(defun isnan (x)
  "Return non nil iff argument X is a NaN."
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

(defun copysign (x1 x2)
  "Copy sign of X2 to value of X1, and return the result.
  Cause an error if X1 or X2 is not a float."
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

(defun frexp (x)
  "Get significand and exponent of a floating point number.
  Breaks the floating point number X into its binary significand SGNFCAND
  (a floating point value between 0.5 (included) and 1.0 (excluded))
  and an integral exponent EXP for 2, such that:
  
    X = SGNFCAND * 2^EXP
  
  The function returns the cons cell (SGNFCAND . EXP).
  If X is zero, both parts (SGNFCAND and EXP) are zero."
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

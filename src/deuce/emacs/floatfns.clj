(ns deuce.emacs.floatfns
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c]
            [deuce.emacs.alloc :as alloc])
  (:refer-clojure :exclude [float]))

(defun ftruncate (arg)
  "Truncate a floating point number to an integral float value.
  Rounds the value toward zero."
  (double (long arg)))

(defun tan (arg)
  "Return the tangent of ARG."
  (Math/tan arg))

(defun atan (y &optional x)
  "Return the inverse tangent of the arguments.
  If only one argument Y is given, return the inverse tangent of Y.
  If two arguments Y and X are given, return the inverse tangent of Y
  divided by X, i.e. the angle in radians between the vector (X, Y)
  and the x-axis."
  (if x
    (Math/atan2 y x)
    (Math/atan y)))

(defun expt (arg1 arg2)
  "Return the exponential ARG1 ** ARG2."
  (Math/pow arg1 arg2))

(defun sqrt (arg)
  "Return the square root of ARG."
  (Math/sqrt arg))

(defun abs (arg)
  "Return the absolute value of ARG."
  (if (float? arg)
    (Math/abs (double arg))
    (Math/abs (long arg))))

(defun ldexp (sgnfcand &optional exponent)
  "Construct number X from significand SGNFCAND and exponent EXP.
  Returns the floating point value resulting from multiplying SGNFCAND
  (the significand) by 2 raised to the power of EXP (the exponent)."
  (* (Math/pow 2 (or exponent 1)) sgnfcand))

(defun fceiling (arg)
  "Return the smallest integer no less than ARG, as a float.
  (Round toward +inf.)"
  (Math/ceil arg))

(defun float (arg)
  "Return the floating point number equal to ARG."
  (double arg))

(defun log (arg &optional base)
  "Return the natural logarithm of ARG.
  If the optional argument BASE is given, return log ARG using that base."
  (/ (Math/log arg) (if base (Math/log base) 1)))

(defun floor (arg &optional divisor)
  "Return the largest integer no greater than ARG.
  This rounds the value towards -inf.
  With optional DIVISOR, return the largest integer no greater than ARG/DIVISOR."
  (long (Math/floor (/ arg (or divisor 1)))))

(defun round (arg &optional divisor)
  "Return the nearest integer to ARG.
  With optional DIVISOR, return the nearest integer to ARG/DIVISOR.

  Rounding a value equidistant between two integers may choose the
  integer closer to zero, or it may prefer an even integer, depending on
  your machine.  For example, (round 2.5) can return 3 on some
  systems, but 2 on others."
  (Math/round (/ arg (double (or divisor 1)))))

(defun log10 (arg)
  "Return the logarithm base 10 of ARG."
  (Math/log10 arg))

(defun isnan (x)
  "Return non nil iff argument X is a NaN."
  (Double/isNaN x))

(defun ffloor (arg)
  "Return the largest integer no greater than ARG, as a float.
  (Round towards -inf.)"
  (double (long arg)))

(defun truncate (arg &optional divisor)
  "Truncate a floating point number to an int.
  Rounds ARG toward zero.
  With optional DIVISOR, truncate ARG/DIVISOR."
  (long (/ arg (or divisor 1))))

(defun ceiling (arg &optional divisor)
  "Return the smallest integer no less than ARG.
  This rounds the value towards +inf.
  With optional DIVISOR, return the smallest integer no less than ARG/DIVISOR."
  (long (Math/ceil (/ arg (or divisor 1)))))

(defun sin (arg)
  "Return the sine of ARG."
  (Math/sin arg))

(defun copysign (x1 x2)
  "Copy sign of X2 to value of X1, and return the result.
  Cause an error if X1 or X2 is not a float."
  (Math/copySign (double x1) (double x2)))

(defun asin (arg)
  "Return the inverse sine of ARG."
  (Math/asin arg))

(defun fround (arg)
  "Return the nearest integer to ARG, as a float."
  (double (long arg)))

(defun acos (arg)
  "Return the inverse cosine of ARG."
  (Math/acos arg))

(defun frexp (x)
  "Get significand and exponent of a floating point number.
  Breaks the floating point number X into its binary significand SGNFCAND
  (a floating point value between 0.5 (included) and 1.0 (excluded))
  and an integral exponent EXP for 2, such that:

    X = SGNFCAND * 2^EXP

  The function returns the cons cell (SGNFCAND . EXP).
  If X is zero, both parts (SGNFCAND and EXP) are zero."
  (if (zero? x)
    (alloc/cons 0.0 0)
    (let [exp (inc (Math/getExponent (double x)))]
      (alloc/cons (/ x (Math/pow 2 exp)) exp))))

(defun exp (arg)
  "Return the exponential base e of ARG."
  (Math/exp arg))

(defun cos (arg)
  "Return the cosine of ARG."
  (Math/cos arg))

(defun logb (arg)
  "Returns largest integer <= the base 2 log of the magnitude of ARG.
  This is the same as the exponent of a float."
  (Math/getExponent (double arg)))

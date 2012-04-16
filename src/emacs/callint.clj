(ns emacs.callint (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun call-interactively (function &optional record-flag keys)
  "Call FUNCTION, reading args according to its interactive calling specs.
  Return the value FUNCTION returns.
  The function contains a specification of how to do the argument reading.
  In the case of user-defined functions, this is specified by placing a call
  to the function `interactive' at the top level of the function body.
  See `interactive'."
  )

(defun prefix-numeric-value (raw)
  "Return numeric meaning of raw prefix argument RAW.
  A raw prefix argument is what you get from `(interactive \"P\")'."
  )

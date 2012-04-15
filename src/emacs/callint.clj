(ns emacs.callint (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun call-interactively (function &optional record-flag keys)
  "Call FUNCTION, reading args according to its interactive calling specs.\nReturn the value FUNCTION returns.\nThe function contains a specification of how to do the argument reading.\nIn the case of user-defined functions, this is specified by placing a call\nto the function `interactive' at the top level of the function body.\nSee `interactive'."
  )

(defun prefix-numeric-value (raw)
  "Return numeric meaning of raw prefix argument RAW.\nA raw prefix argument is what you get from `(interactive \"P\")'.\n"
  )

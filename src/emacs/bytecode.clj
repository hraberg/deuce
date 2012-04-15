(ns emacs.bytecode (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun byte-code (bytestr vector maxdepth)
  "Function used internally in byte-compiled code.\nThe first argument, BYTESTR, is a string of byte code;\nthe second, VECTOR, a vector of constants;\nthe third, MAXDEPTH, the maximum stack depth used in this function.\n"
  )

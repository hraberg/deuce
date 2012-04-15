(ns emacs.undo (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun undo-boundary ()
  "Mark a boundary between units of undo.\nAn undo command will stop at this point,\n"
  )

(defun primitive-undo (n list)
  "Undo N records from the front of the list LIST.\n"
  )

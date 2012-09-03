(ns
 deuce.emacs.xml
 (use [deuce.emacs-lisp :only (defun defvar)])
 (require [clojure.core :as c])
 (:refer-clojure :exclude []))

(defun libxml-parse-html-region (start end &optional base-url)
  "Parse the region as an HTML document and return the parse tree.
  If BASE-URL is non-nil, it is used to expand relative URLs."
  )

(defun libxml-parse-xml-region (start end &optional base-url)
  "Parse the region as an XML document and return the parse tree.
  If BASE-URL is non-nil, it is used to expand relative URLs."
  )

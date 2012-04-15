(ns emacs.dired (use [deuce.core]) (:refer-clojure :only []))

(defun file-attributes (filename &optional id-format)
  "Return a list of attributes of file FILENAME.\nValue is nil if specified file cannot be opened."
  )

(defun directory-files (directory &optional full match nosort)
  "Return a list of names of files in DIRECTORY.\nThere are three optional arguments:\nIf FULL is non-nil, return absolute file names.  Otherwise return names\n that are relative to the specified directory.\nIf MATCH is non-nil, mention only file names that match the regexp MATCH.\nIf NOSORT is non-nil, the list is not sorted--its order is unpredictable.\n Otherwise, the list returned is sorted with `string-lessp'.\n"
  )

(defun file-name-completion (file directory &optional predicate)
  "Complete file name FILE in directory DIRECTORY.\nReturns the longest string\ncommon to all file names in DIRECTORY that start with FILE.\nIf there is only one and FILE matches it exactly, returns t.\nReturns nil if DIRECTORY contains no name starting with FILE."
  )

(defun file-name-all-completions (file directory)
  "Return a list of all completions of file name FILE in directory DIRECTORY.\nThese are all file names in directory DIRECTORY which begin with FILE.internal-set-alternative-font-family-alist is a built-in function in\n`C source code'."
  )

(defun file-attributes-lessp (f1 f2)
  "Return t if first arg file attributes list is less than second.\n"
  )

(ns emacs.dired (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun file-attributes (filename &optional id-format)
  "Return a list of attributes of file FILENAME.
  Value is nil if specified file cannot be opened."
  )

(defun directory-files (directory &optional full match nosort)
  "Return a list of names of files in DIRECTORY.
  There are three optional arguments:
  If FULL is non-nil, return absolute file names.  Otherwise return names
   that are relative to the specified directory.
  If MATCH is non-nil, mention only file names that match the regexp MATCH.
  If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
   Otherwise, the list returned is sorted with `string-lessp'.
   NOSORT is useful if you plan to sort the result yourself."
  )

(defun file-name-completion (file directory &optional predicate)
  "Complete file name FILE in directory DIRECTORY.
  Returns the longest string
  common to all file names in DIRECTORY that start with FILE.
  If there is only one and FILE matches it exactly, returns t.
  Returns nil if DIRECTORY contains no name starting with FILE."
  )

(defun file-name-all-completions (file directory)
  "Return a list of all completions of file name FILE in directory DIRECTORY.
  These are all file names in directory DIRECTORY which begin with FILE."
  )

(defun file-attributes-lessp (f1 f2)
  "Return t if first arg file attributes list is less than second.
  Comparison is in lexicographic order and case is significant."
  )

(defun directory-files-and-attributes (directory &optional full match nosort id-format)
  "Return a list of names of files and their attributes in DIRECTORY.
  There are four optional arguments:
  If FULL is non-nil, return absolute file names.  Otherwise return names
   that are relative to the specified directory.
  If MATCH is non-nil, mention only file names that match the regexp MATCH.
  If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
   NOSORT is useful if you plan to sort the result yourself.
  ID-FORMAT specifies the preferred format of attributes uid and gid, see
  `file-attributes' for further documentation.
  On MS-Windows, performance depends on `w32-get-true-file-attributes',
  which see."
  )

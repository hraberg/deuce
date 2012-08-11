(ns
 deuce.emacs.dired
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun file-attributes (filename &optional id-format)
  "Return a list of attributes of file FILENAME.
  Value is nil if specified file cannot be opened.
  
  ID-FORMAT specifies the preferred format of attributes uid and gid (see
  below) - valid values are 'string and 'integer.  The latter is the
  default, but we plan to change that, so you should specify a non-nil value
  for ID-FORMAT if you use the returned uid or gid.
  
  Elements of the attribute list are:
   0. t for directory, string (name linked to) for symbolic link, or nil.
   1. Number of links to file.
   2. File uid as a string or a number.  If a string value cannot be
    looked up, a numeric value, either an integer or a float, is returned.
   3. File gid, likewise.
   4. Last access time, as a list of two integers.
    First integer has high-order 16 bits of time, second has low 16 bits.
    (See a note below about access time on FAT-based filesystems.)
   5. Last modification time, likewise.  This is the time of the last
    change to the file's contents.
   6. Last status change time, likewise.  This is the time of last change
    to the file's attributes: owner and group, access mode bits, etc.
   7. Size in bytes.
    This is a floating point number if the size is too large for an integer.
   8. File modes, as a string of ten letters or dashes as in ls -l.
   9. t if file's gid would change if file were deleted and recreated.
  10. inode number.  If it is larger than what an Emacs integer can hold,
    this is of the form (HIGH . LOW): first the high bits, then the low 16 bits.
    If even HIGH is too large for an Emacs integer, this is instead of the form
    (HIGH MIDDLE . LOW): first the high bits, then the middle 24 bits,
    and finally the low 16 bits.
  11. Filesystem device number.  If it is larger than what the Emacs
    integer can hold, this is a cons cell, similar to the inode number.
  
  On most filesystems, the combination of the inode and the device
  number uniquely identifies the file.
  
  On MS-Windows, performance depends on `w32-get-true-file-attributes',
  which see.
  
  On some FAT-based filesystems, only the date of last access is recorded,
  so last access time will always be midnight of that day."
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
  Returns nil if DIRECTORY contains no name starting with FILE.
  
  If PREDICATE is non-nil, call PREDICATE with each possible
  completion (in absolute form) and ignore it if PREDICATE returns nil.
  
  This function ignores some of the possible completions as
  determined by the variable `completion-ignored-extensions', which see."
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

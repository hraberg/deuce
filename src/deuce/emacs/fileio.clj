(ns
 deuce.emacs.fileio
 (use [deuce.emacs-lisp :only (defun)])
 (:refer-clojure :exclude []))

(defun clear-buffer-auto-save-failure ()
  "Clear any record of a recent auto-save failure in the current buffer."
  )

(defun file-name-absolute-p (filename)
  "Return t if file FILENAME specifies an absolute file name.
  On Unix, this is a name starting with a `/' or a `~'."
  )

(defun set-visited-file-modtime (&optional time-list)
  "Update buffer's recorded modification time from the visited file's time.
  Useful if the buffer was not read from the file normally
  or if the file itself has been changed for some known benign reason.
  An argument specifies the modification time value to use
  (instead of that of the visited file), in the form of a list
  (HIGH . LOW) or (HIGH LOW)."
  )

(defun set-default-file-modes (mode)
  "Set the file permission bits for newly created files.
  The argument MODE should be an integer; only the low 9 bits are used.
  This setting is inherited by subprocesses."
  )

(defun file-writable-p (filename)
  "Return t if file FILENAME can be written or created by you."
  )

(defun car-less-than-car (a b)
  "Return t if (car A) is numerically less than (car B)."
  )

(defun expand-file-name (name &optional default-directory)
  "Convert filename NAME to absolute, and canonicalize it.
  Second arg DEFAULT-DIRECTORY is directory to start with if NAME is relative
  (does not start with slash or tilde); if DEFAULT-DIRECTORY is nil or missing,
  the current buffer's value of `default-directory' is used.
  NAME should be a string that is a valid file name for the underlying
  filesystem.
  File name components that are `.' are removed, and
  so are file name components followed by `..', along with the `..' itself;
  note that these simplifications are done without checking the resulting
  file names in the file system.
  Multiple consecutive slashes are collapsed into a single slash,
  except at the beginning of the file name when they are significant (e.g.,
  UNC file names on MS-Windows.)
  An initial `~/' expands to your home directory.
  An initial `~USER/' expands to USER's home directory.
  See also the function `substitute-in-file-name'."
  )

(defun write-region (start end filename &optional append visit lockname mustbenew)
  "Write current region into specified file.
  When called from a program, requires three arguments:
  START, END and FILENAME.  START and END are normally buffer positions
  specifying the part of the buffer to write.
  If START is nil, that means to use the entire buffer contents.
  If START is a string, then output that string to the file
  instead of any buffer contents; END is ignored."
  )

(defun file-exists-p (filename)
  "Return t if file FILENAME exists (whether or not you can read it.)
  See also `file-readable-p' and `file-attributes'.
  This returns nil for a symlink to a nonexistent file.
  Use `file-symlink-p' to test for such links."
  )

(defun do-auto-save (&optional no-message current-only)
  "Auto-save all buffers that need it.
  This is all buffers that have auto-saving enabled
  and are changed since last auto-saved.
  Auto-saving writes the buffer into a file
  so that your editing is not lost if the system crashes.
  This file is not the file you visited; that changes only when you save.
  Normally we run the normal hook `auto-save-hook' before saving."
  )

(defun delete-directory-internal (directory)
  "Delete the directory named DIRECTORY.  Does not follow symlinks."
  )

(defun find-file-name-handler (filename operation)
  "Return FILENAME's handler function for OPERATION, if it has one.
  Otherwise, return nil.
  A file name is handled if one of the regular expressions in
  `file-name-handler-alist' matches it."
  )

(defun rename-file (file newname &optional ok-if-already-exists)
  "Rename FILE as NEWNAME.  Both args must be strings.
  If file has names other than FILE, it continues to have those names.
  Signals a `file-already-exists' error if a file NEWNAME already exists
  unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
  A number as third arg means request confirmation if NEWNAME already exists.
  This is what happens in interactive use with M-x."
  )

(defun make-symbolic-link (filename linkname &optional ok-if-already-exists)
  "Make a symbolic link to FILENAME, named LINKNAME.
  Both args must be strings.
  Signals a `file-already-exists' error if a file LINKNAME already exists
  unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
  A number as third arg means request confirmation if LINKNAME already exists.
  This happens for interactive use with M-x."
  )

(defun default-file-modes ()
  "Return the default file protection for created files.
  The value is an integer."
  )

(defun clear-visited-file-modtime ()
  "Clear out records of last mod time of visited file.
  Next attempt to save will certainly not complain of a discrepancy."
  )

(defun visited-file-modtime ()
  "Return the current buffer's recorded visited file modification time.
  The value is a list of the form (HIGH LOW), like the time values
  that `file-attributes' returns.  If the current buffer has no recorded
  file modification time, this function returns 0.
  See Info node `(elisp)Modification Time' for more details."
  )

(defun unix-sync ()
  "Tell Unix to finish all pending disk updates."
  )

(defun verify-visited-file-modtime (buf)
  "Return t if last mod time of BUF's visited file matches what BUF records.
  This means that the file has not been changed since it was visited or saved.
  See Info node `(elisp)Modification Time' for more details."
  )

(defun directory-file-name (directory)
  "Returns the file name of the directory named DIRECTORY.
  This is the name of the file that holds the data for the directory DIRECTORY.
  This operation exists because a directory is also a file, but its name as
  a directory is different from its name as a file.
  In Unix-syntax, this function just removes the final slash."
  )

(defun make-directory-internal (directory)
  "Create a new directory named DIRECTORY."
  )

(defun file-newer-than-file-p (file1 file2)
  "Return t if file FILE1 is newer than file FILE2.
  If FILE1 does not exist, the answer is nil;
  otherwise, if FILE2 does not exist, the answer is t."
  )

(defun file-readable-p (filename)
  "Return t if file FILENAME exists and you can read it.
  See also `file-exists-p' and `file-attributes'."
  )

(defun delete-file (filename)
  "Delete file named FILENAME.  If it is a symlink, remove the symlink.
  If file has multiple names, it continues to exist with the other names."
  )

(defun file-executable-p (filename)
  "Return t if FILENAME can be executed by you.
  For a directory, this means you can access files in that directory."
  )

(defun make-temp-name (prefix)
  "Generate temporary file name (string) starting with PREFIX (a string).
  The Emacs process number forms part of the result,
  so there is no danger of generating a name being used by another process."
  )

(defun access-file (filename string)
  "Access file FILENAME, and get an error if that does not work.
  The second argument STRING is used in the error message.
  If there is no error, returns nil."
  )

(defun add-name-to-file (file newname &optional ok-if-already-exists)
  "Give FILE additional name NEWNAME.  Both args must be strings.
  Signals a `file-already-exists' error if a file NEWNAME already exists
  unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
  A number as third arg means request confirmation if NEWNAME already exists.
  This is what happens in interactive use with M-x."
  )

(defun recent-auto-save-p ()
  "Return t if current buffer has been auto-saved recently.
  More precisely, if it has been auto-saved since last read from or saved
  in the visited file.  If the buffer has no visited file,
  then any auto-save counts as \"recent\"."
  )

(defun insert-file-contents (filename &optional visit beg end replace)
  "Insert contents of file FILENAME after point.
  Returns list of absolute file name and number of characters inserted.
  If second argument VISIT is non-nil, the buffer's visited filename and
  last save file modtime are set, and it is marked unmodified.  If
  visiting and the file does not exist, visiting is completed before the
  error is signaled."
  )

(defun file-name-nondirectory (filename)
  "Return file name FILENAME sans its directory.
  For example, in a Unix-syntax file name,
  this is everything after the last slash,
  or the entire name if it contains no slash."
  )

(defun substitute-in-file-name (filename)
  "Substitute environment variables referred to in FILENAME.
  `$FOO' where FOO is an environment variable name means to substitute
  the value of that variable.  The variable name should be terminated
  with a character not a letter, digit or underscore; otherwise, enclose
  the entire variable name in braces."
  )

(defun file-directory-p (filename)
  "Return t if FILENAME names an existing directory.
  Symbolic links to directories count as directories.
  See `file-symlink-p' to distinguish symlinks."
  )

(defun set-buffer-auto-saved ()
  "Mark current buffer as auto-saved with its current text.
  No auto-save file will be written until the buffer changes again."
  )

(defun set-file-times (filename &optional time)
  "Set times of file FILENAME to TIME.
  Set both access and modification times.
  Return t on success, else nil.
  Use the current time if TIME is nil.  TIME is in the format of
  `current-time'."
  )

(defun set-file-modes (filename mode)
  "Set mode bits of file named FILENAME to MODE (an integer).
  Only the 12 low bits of MODE are used."
  )

(defun file-accessible-directory-p (filename)
  "Return t if file FILENAME names a directory you can open.
  For the value to be t, FILENAME must specify the name of a directory as a file,
  and the directory must allow you to open files in it.  In order to use a
  directory as a buffer's current directory, this predicate must return true.
  A directory name spec may be given instead; then the value is t
  if the directory so specified exists and really is a readable and
  searchable directory."
  )

(defun file-name-as-directory (file)
  "Return a string representing the file name FILE interpreted as a directory.
  This operation exists because a directory is also a file, but its name as
  a directory is different from its name as a file.
  The result can be used as the value of `default-directory'
  or passed as second argument to `expand-file-name'.
  For a Unix-syntax file name, just appends a slash."
  )

(defun copy-file (file newname &optional ok-if-already-exists keep-time preserve-uid-gid)
  "Copy FILE to NEWNAME.  Both args must be strings.
  If NEWNAME names a directory, copy FILE there."
  )

(defun file-regular-p (filename)
  "Return t if FILENAME names a regular file.
  This is the sort of file that holds an ordinary stream of data bytes.
  Symbolic links to regular files count as regular files.
  See `file-symlink-p' to distinguish symlinks."
  )

(defun file-name-directory (filename)
  "Return the directory component in file name FILENAME.
  Return nil if FILENAME does not include a directory.
  Otherwise return a directory name.
  Given a Unix syntax file name, returns a string ending in slash."
  )

(defun file-symlink-p (filename)
  "Return non-nil if file FILENAME is the name of a symbolic link.
  The value is the link target, as a string.
  Otherwise it returns nil."
  )

(defun unhandled-file-name-directory (filename)
  "Return a directly usable directory name somehow associated with FILENAME.
  A `directly usable' directory name is one that may be used without the
  intervention of any file handler.
  If FILENAME is a directly usable file itself, return
  (file-name-directory FILENAME).
  If FILENAME refers to a file which is not accessible from a local process,
  then this should return nil.
  The `call-process' and `start-process' functions use this function to
  get a current directory to run processes in."
  )

(defun file-modes (filename)
  "Return mode bits of file named FILENAME, as an integer.
  Return nil, if file does not exist or is not accessible."
  )

(defun next-read-file-uses-dialog-p ()
  "Return t if a call to `read-file-name' will use a dialog.
  The return value is only relevant for a call to `read-file-name' that happens
  before any other event (mouse or keypress) is handled."
  )

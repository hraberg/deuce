(ns emacs.fileio (use [deuce.core]) (:refer-clojure :only []))

(defun file-name-absolute-p (filename)
  "Return t if file FILENAME specifies an absolute file name.\n"
  )

(defun set-visited-file-modtime (&optional time-list)
  "Update buffer's recorded modification time from the visited file's time.\nUseful if the buffer was not read from the file normally\nor if the file itself has been changed for some known benign reason.\nAn argument specifies the modification time value to use\n(instead of that of the visited file), in the form of a list\n"
  )

(defun set-default-file-modes (mode)
  "Set the file permission bits for newly created files.\nThe argument MODE should be an integer; only the low 9 bits are used.\nThis setting is inherited by subprocesses.suspend-emacs is an interactive built-in function in `C source code'."
  )

(defun file-writable-p (filename)
  )

(defun car-less-than-car (a b)
  )

(defun expand-file-name (name &optional default-directory)
  "Convert filename NAME to absolute, and canonicalize it.\nSecond arg DEFAULT-DIRECTORY is directory to start with if NAME is relative\n(does not start with slash or tilde); if DEFAULT-DIRECTORY is nil or missing,\nthe current buffer's value of `default-directory' is used.\nNAME should be a string that is a valid file name for the underlying\nfilesystem.\nFile name components that are `.' are removed, and\nso are file name components followed by `..', along with the `..' itself;\nnote that these simplifications are done without checking the resulting\nfile names in the file system.\nMultiple consecutive slashes are collapsed into a single slash,\nexcept at the beginning of the file name when they are significant (e.g.,\nUNC file names on MS-Windows.)\nAn initial `~/' expands to your home directory.\nAn initial `~USER/' expands to USER's home directory.\nSee also the function `substitute-in-file-name'."
  )

(defun file-exists-p (filename)
  "Return t if file FILENAME exists (whether or not you can read it.)\nSee also `file-readable-p' and `file-attributes'.\nThis returns nil for a symlink to a nonexistent file.\n"
  )

(defun delete-directory-internal (directory)
  )

(defun find-file-name-handler (filename operation)
  "Return FILENAME's handler function for OPERATION, if it has one.\nOtherwise, return nil.\nA file name is handled if one of the regular expressions in\n`file-name-handler-alist' matches it."
  )

(defun default-file-modes ()
  "Return the default file protection for created files.\nThe value is an integer.end-of-line is an interactive built-in function in `C source code'."
  )

(defun clear-visited-file-modtime ()
  "Clear out records of last mod time of visited file.\n"
  )

(defun visited-file-modtime ()
  "Return the current buffer's recorded visited file modification time.\nThe value is a list of the form (HIGH LOW), like the time values\nthat `file-attributes' returns.  If the current buffer has no recorded\nfile modification time, this function returns 0.\nSee Info node `(elisp)Modification Time' for more details.current-window-configuration is a built-in function in `C source\ncode'."
  )

(defun verify-visited-file-modtime (buf)
  "Return t if last mod time of BUF's visited file matches what BUF records.\nThis means that the file has not been changed since it was visited or saved.\n"
  )

(defun directory-file-name (directory)
  "Returns the file name of the directory named DIRECTORY.\nThis is the name of the file that holds the data for the directory DIRECTORY.\nThis operation exists because a directory is also a file, but its name as\na directory is different from its name as a file.\nIn Unix-syntax, this function just removes the final slash.decode-coding-region is an interactive built-in function in `C source\ncode'."
  )

(defun make-directory-internal (directory)
  "Create a new directory named DIRECTORY.beginning-of-line is an interactive built-in function in `C source\ncode'."
  )

(defun file-newer-than-file-p (file1 file2)
  "Return t if file FILE1 is newer than file FILE2.\nIf FILE1 does not exist, the answer is nil;\n"
  )

(defun file-readable-p (filename)
  "Return t if file FILENAME exists and you can read it.\n"
  )

(defun file-executable-p (filename)
  "Return t if FILENAME can be executed by you.\n"
  )

(defun make-temp-name (prefix)
  "Generate temporary file name (string) starting with PREFIX (a string).\nThe Emacs process number forms part of the result,\nso there is no danger of generating a name being used by another process."
  )

(defun access-file (filename string)
  "Access file FILENAME, and get an error if that does not work.\nThe second argument STRING is used in the error message.\n"
  )

(defun recent-auto-save-p ()
  "Return t if current buffer has been auto-saved recently.\nMore precisely, if it has been auto-saved since last read from or saved\nin the visited file.  If the buffer has no visited file,\n"
  )

(defun insert-file-contents (filename &optional visit beg end replace)
  "Insert contents of file FILENAME after point.\nReturns list of absolute file name and number of characters inserted.\nIf second argument VISIT is non-nil, the buffer's visited filename and\nlast save file modtime are set, and it is marked unmodified.  If\nvisiting and the file does not exist, visiting is completed before the\nerror is signaled."
  )

(defun file-name-nondirectory (filename)
  "Return file name FILENAME sans its directory.\nFor example, in a Unix-syntax file name,\nthis is everything after the last slash,\n"
  )

(defun substitute-in-file-name (filename)
  "Substitute environment variables referred to in FILENAME.\n`$FOO' where FOO is an environment variable name means to substitute\nthe value of that variable.  The variable name should be terminated\nwith a character not a letter, digit or underscore; otherwise, enclose\nthe entire variable name in braces."
  )

(defun file-directory-p (filename)
  "Return t if FILENAME names an existing directory.\nSymbolic links to directories count as directories.\n"
  )

(defun set-buffer-auto-saved ()
  "Mark current buffer as auto-saved with its current text.\n"
  )

(defun set-file-times (filename &optional time)
  "Set times of file FILENAME to TIME.\nSet both access and modification times.\nReturn t on success, else nil.\nUse the current time if TIME is nil.  TIME is in the format of\n"
  )

(defun file-accessible-directory-p (filename)
  "Return t if file FILENAME names a directory you can open.\nFor the value to be t, FILENAME must specify the name of a directory as a file,\nand the directory must allow you to open files in it.  In order to use a\ndirectory as a buffer's current directory, this predicate must return true.\nA directory name spec may be given instead; then the value is t\nif the directory so specified exists and really is a readable and\n"
  )

(defun file-name-as-directory (file)
  "Return a string representing the file name FILE interpreted as a directory.\nThis operation exists because a directory is also a file, but its name as\na directory is different from its name as a file.\nThe result can be used as the value of `default-directory'\nor passed as second argument to `expand-file-name'.\n"
  )

(defun file-regular-p (filename)
  "Return t if FILENAME names a regular file.\nThis is the sort of file that holds an ordinary stream of data bytes.\nSymbolic links to regular files count as regular files.\nSee `file-symlink-p' to distinguish symlinks.condition-case is a special form in `C source code'."
  )

(defun file-name-directory (filename)
  "Return the directory component in file name FILENAME.\nReturn nil if FILENAME does not include a directory.\nOtherwise return a directory name.\n"
  )

(defun file-symlink-p (filename)
  "Return non-nil if file FILENAME is the name of a symbolic link.\nThe value is the link target, as a string.\nOtherwise it returns nil."
  )

(defun file-modes (filename)
  "Return mode bits of file named FILENAME, as an integer.\n"
  )

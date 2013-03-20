(ns deuce.emacs.fileio
  (:use [deuce.emacs-lisp :only (defun defvar) :as el])
  (:require [clojure.core :as c]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [deuce.emacs.buffer :as buffer]
            [deuce.emacs.data :as data]
            [deuce.emacs.eval :as eval]
            [deuce.emacs.editfns :as editfns])
  (:import [java.nio.file Files LinkOption
            NoSuchFileException])
  (:refer-clojure :exclude []))

(defvar file-name-coding-system nil
  "*Coding system for encoding file names.
  If it is nil, `default-file-name-coding-system' (which see) is used.")

(defvar set-auto-coding-function nil
  "If non-nil, a function to call to decide a coding system of file.
  Two arguments are passed to this function: the file name
  and the length of a file contents following the point.
  This function should return a coding system to decode the file contents.
  It should check the file name against `auto-coding-alist'.
  If no coding system is decided, it should check a coding system
  specified in the heading lines with the format:
  	-*- ... coding: CODING-SYSTEM; ... -*-
  or local variable spec of the tailing lines with `coding:' tag.")

(defvar delete-by-moving-to-trash nil
  "Specifies whether to use the system's trash can.
  When non-nil, certain file deletion commands use the function
  `move-file-to-trash' instead of deleting files outright.
  This includes interactive calls to `delete-file' and
  `delete-directory' and the Dired deletion commands.

  You can customize this variable.")

(defvar auto-save-list-file-name nil
  "File name in which we write a list of all auto save file names.
  This variable is initialized automatically from `auto-save-list-file-prefix'
  shortly after Emacs reads your `.emacs' file, if you have not yet given it
  a non-nil value.")

(defvar default-file-name-coding-system nil
  "Default coding system for encoding file names.
  This variable is used only when `file-name-coding-system' is nil.

  This variable is set/changed by the command `set-language-environment'.
  User should not set this variable manually,
  instead use `file-name-coding-system' to get a constant encoding
  of file names regardless of the current language environment.")

(defvar write-region-annotations-so-far nil
  "When an annotation function is called, this holds the previous annotations.
  These are the annotations made by other annotation functions
  that were already called.  See also `write-region-annotate-functions'.")

(defvar inhibit-file-name-operation nil
  "The operation for which `inhibit-file-name-handlers' is applicable.")

(defvar write-region-inhibit-fsync nil
  "*Non-nil means don't call fsync in `write-region'.
  This variable affects calls to `write-region' as well as save commands.
  A non-nil value may result in data loss!")

(defvar auto-save-visited-file-name nil
  "Non-nil says auto-save a buffer in the file it is visiting, when practical.
  Normally auto-save files are written under other names.

  You can customize this variable.")

(defvar write-region-annotate-functions nil
  "A list of functions to be called at the start of `write-region'.
  Each is passed two arguments, START and END as for `write-region'.
  These are usually two numbers but not always; see the documentation
  for `write-region'.  The function should return a list of pairs
  of the form (POSITION . STRING), consisting of strings to be effectively
  inserted at the specified positions of the file being written (1 means to
  insert before the first byte written).  The POSITIONs must be sorted into
  increasing order.

  If there are several annotation functions, the lists returned by these
  functions are merged destructively.  As each annotation function runs,
  the variable `write-region-annotations-so-far' contains a list of all
  annotations returned by previous annotation functions.

  An annotation function can return with a different buffer current.
  Doing so removes the annotations returned by previous functions, and
  resets START and END to `point-min' and `point-max' of the new buffer.

  After `write-region' completes, Emacs calls the function stored in
  `write-region-post-annotation-function', once for each buffer that was
  current when building the annotations (i.e., at least once), with that
  buffer current.")

(defvar file-name-handler-alist nil
  "Alist of elements (REGEXP . HANDLER) for file names handled specially.
  If a file name matches REGEXP, all I/O on that file is done by calling
  HANDLER.  If a file name matches more than one handler, the handler
  whose match starts last in the file name gets precedence.  The
  function `find-file-name-handler' checks this list for a handler for
  its argument.

  HANDLER should be a function.  The first argument given to it is the
  name of the I/O primitive to be handled; the remaining arguments are
  the arguments that were passed to that primitive.  For example, if you
  do (file-exists-p FILENAME) and FILENAME is handled by HANDLER, then
  HANDLER is called like this:

    (funcall HANDLER 'file-exists-p FILENAME)

  Note that HANDLER must be able to handle all I/O primitives; if it has
  nothing special to do for a primitive, it should reinvoke the
  primitive to handle the operation \"the usual way\".
  See Info node `(elisp)Magic File Names' for more details.")

(defvar auto-save-include-big-deletions nil
  "If non-nil, auto-save even if a large part of the text is deleted.
  If nil, deleting a substantial portion of the text disables auto-save
  in the buffer; this is the default behavior, because the auto-save
  file is usually more useful if it contains the deleted text.")

(defvar after-insert-file-functions nil
  "A list of functions to be called at the end of `insert-file-contents'.
  Each is passed one argument, the number of characters inserted,
  with point at the start of the inserted text.  Each function
  should leave point the same, and return the new character count.
  If `insert-file-contents' is intercepted by a handler from
  `file-name-handler-alist', that handler is responsible for calling the
  functions in `after-insert-file-functions' if appropriate.")

(defvar inhibit-file-name-handlers nil
  "A list of file name handlers that temporarily should not be used.
  This applies only to the operation `inhibit-file-name-operation'.")

(defvar write-region-post-annotation-function nil
  "Function to call after `write-region' completes.
  The function is called with no arguments.  If one or more of the
  annotation functions in `write-region-annotate-functions' changed the
  current buffer, the function stored in this variable is called for
  each of those additional buffers as well, in addition to the original
  buffer.  The relevant buffer is current during each function call.")

(defun clear-buffer-auto-save-failure ()
  "Clear any record of a recent auto-save failure in the current buffer."
  )

(defun file-name-absolute-p (filename)
  "Return t if file FILENAME specifies an absolute file name.
  On Unix, this is a name starting with a `/' or a `~'."
  (el/check-type 'stringp filename)
  (.isAbsolute (io/file filename)))

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
  (el/check-type 'stringp filename)
  (Files/isWritable (.toPath (io/file filename))))

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
  See also the function `substitute-in-file-name'.

  For technical reasons, this function can return correct but
  non-intuitive results for the root directory; for instance,
  (expand-file-name \"..\" \"/\") returns \"/..\".  For this reason, use
  (directory-file-name (file-name-directory dirname)) to traverse a
  filesystem tree, not (expand-file-name \"..\"  dirname)."
  (el/check-type 'stringp name)
  (let [default-directory (or default-directory (data/symbol-value 'default-directory))]
    (let [file (io/file (s/replace name #"^~" (System/getProperty "user.home")))]
      (.getAbsolutePath
       (if (.isAbsolute file)
         file
         (io/file default-directory name))))))

(defun write-region (start end filename &optional append visit lockname mustbenew)
  "Write current region into specified file.
  When called from a program, requires three arguments:
  START, END and FILENAME.  START and END are normally buffer positions
  specifying the part of the buffer to write.
  If START is nil, that means to use the entire buffer contents.
  If START is a string, then output that string to the file
  instead of any buffer contents; END is ignored.

  Optional fourth argument APPEND if non-nil means
    append to existing file contents (if any).  If it is an integer,
    seek to that offset in the file before writing.
  Optional fifth argument VISIT, if t or a string, means
    set the last-save-file-modtime of buffer to this file's modtime
    and mark buffer not modified.
  If VISIT is a string, it is a second file name;
    the output goes to FILENAME, but the buffer is marked as visiting VISIT.
    VISIT is also the file name to lock and unlock for clash detection.
  If VISIT is neither t nor nil nor a string,
    that means do not display the \"Wrote file\" message.
  The optional sixth arg LOCKNAME, if non-nil, specifies the name to
    use for locking and unlocking, overriding FILENAME and VISIT.
  The optional seventh arg MUSTBENEW, if non-nil, insists on a check
    for an existing file with the same name.  If MUSTBENEW is `excl',
    that means to get an error if the file already exists; never overwrite.
    If MUSTBENEW is neither nil nor `excl', that means ask for
    confirmation before overwriting, but do go ahead and overwrite the file
    if the user confirms.

  This does code conversion according to the value of
  `coding-system-for-write', `buffer-file-coding-system', or
  `file-coding-system-alist', and sets the variable
  `last-coding-system-used' to the coding system actually used.

  This calls `write-region-annotate-functions' at the start, and
  `write-region-post-annotation-function' at the end."
  )

(defun file-exists-p (filename)
  "Return t if file FILENAME exists (whether or not you can read it.)
  See also `file-readable-p' and `file-attributes'.
  This returns nil for a symlink to a nonexistent file.
  Use `file-symlink-p' to test for such links."
  (.exists (io/file filename)))

(defun set-file-selinux-context (filename context)
  "Set SELinux context of file named FILENAME to CONTEXT.
  CONTEXT should be a list (USER ROLE TYPE RANGE), where the list
  elements are strings naming the components of a SELinux context.

  This function does nothing if SELinux is disabled, or if Emacs was not
  compiled with SELinux support."
  )

(defun do-auto-save (&optional no-message current-only)
  "Auto-save all buffers that need it.
  This is all buffers that have auto-saving enabled
  and are changed since last auto-saved.
  Auto-saving writes the buffer into a file
  so that your editing is not lost if the system crashes.
  This file is not the file you visited; that changes only when you save.
  Normally we run the normal hook `auto-save-hook' before saving.

  A non-nil NO-MESSAGE argument means do not print any message if successful.
  A non-nil CURRENT-ONLY argument means save only current buffer."
  )

(defun file-selinux-context (filename)
  "Return SELinux context of file named FILENAME.
  The return value is a list (USER ROLE TYPE RANGE), where the list
  elements are strings naming the user, role, type, and range of the
  file's SELinux security context.

  Return (nil nil nil nil) if the file is nonexistent or inaccessible,
  or if SELinux is disabled, or if Emacs lacks SELinux support."
  )

(defun delete-directory-internal (directory)
  "Delete the directory named DIRECTORY.  Does not follow symlinks."
  )

(defun find-file-name-handler (filename operation)
  "Return FILENAME's handler function for OPERATION, if it has one.
  Otherwise, return nil.
  A file name is handled if one of the regular expressions in
  `file-name-handler-alist' matches it.

  If OPERATION equals `inhibit-file-name-operation', then we ignore
  any handlers that are members of `inhibit-file-name-handlers',
  but we still do run any other handlers.  This lets handlers
  use the standard functions without calling themselves recursively."
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
  The value is a list of the form (HIGH LOW), like the time values that
  `file-attributes' returns.  If the current buffer has no recorded file
  modification time, this function returns 0.  If the visited file
  doesn't exist, HIGH will be -1.
  See Info node `(elisp)Modification Time' for more details."
  )

(defun unix-sync ()
  "Tell Unix to finish all pending disk updates."
  )

(defun verify-visited-file-modtime (&optional buf)
  "Return t if last mod time of BUF's visited file matches what BUF records.
  This means that the file has not been changed since it was visited or saved.
  If BUF is omitted or nil, it defaults to the current buffer.
  See Info node `(elisp)Modification Time' for more details."
  )

(defun directory-file-name (directory)
  "Returns the file name of the directory named DIRECTORY.
  This is the name of the file that holds the data for the directory DIRECTORY.
  This operation exists because a directory is also a file, but its name as
  a directory is different from its name as a file.
  In Unix-syntax, this function just removes the final slash."
  (el/check-type 'stringp directory)
  (if (= "/" directory)
    directory
    (if (re-find #"/$" directory)
      (subs directory 0 (dec (count directory)))
      directory)))

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
  (el/check-type 'stringp filename)
  (Files/isReadable (.toPath (io/file filename))))

(defun delete-file (filename &optional trash)
  "Delete file named FILENAME.  If it is a symlink, remove the symlink.
  If file has multiple names, it continues to exist with the other names.
  TRASH non-nil means to trash the file instead of deleting, provided
  `delete-by-moving-to-trash' is non-nil.

  When called interactively, TRASH is t if no prefix argument is given.
  With a prefix argument, TRASH is nil."
  )

(defun file-executable-p (filename)
  "Return t if FILENAME can be executed by you.
  For a directory, this means you can access files in that directory."
  (el/check-type 'stringp filename)
  (Files/isExecutable (.toPath (io/file filename))))

(defun make-temp-name (prefix)
  "Generate temporary file name (string) starting with PREFIX (a string).
  The Emacs process number forms part of the result,
  so there is no danger of generating a name being used by another process.

  In addition, this function makes an attempt to choose a name
  which has no existing file.  To make this work,
  PREFIX should be an absolute file name.

  There is a race condition between calling `make-temp-name' and creating the
  file which opens all kinds of security holes.  For that reason, you should
  probably use `make-temp-file' instead, except in three circumstances:

  * If you are creating the file in the user's home directory.
  * If you are creating a directory rather than an ordinary file.
  * If you are taking special precautions as `make-temp-file' does."
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
  error is signaled.

  The optional third and fourth arguments BEG and END specify what portion
  of the file to insert.  These arguments count bytes in the file, not
  characters in the buffer.  If VISIT is non-nil, BEG and END must be nil.

  If optional fifth argument REPLACE is non-nil, replace the current
  buffer contents (in the accessible portion) with the file contents.
  This is better than simply deleting and inserting the whole thing
  because (1) it preserves some marker positions and (2) it puts less data
  in the undo list.  When REPLACE is non-nil, the second return value is
  the number of characters that replace previous buffer contents.

  This function does code conversion according to the value of
  `coding-system-for-read' or `file-coding-system-alist', and sets the
  variable `last-coding-system-used' to the coding system actually used."
  (let [file (let [file (io/file filename)]
               (if (.exists file)
                 (.toURL file)
                 (io/resource filename)))
        contents (slurp file)
        contents (if (and visit beg end)
                   (subs contents beg end)
                   contents)
        path (.getPath file)
        point (editfns/point)]
    (editfns/insert contents)
    (when visit
      (reset! (.save-modiff (.text (buffer/current-buffer))) (System/currentTimeMillis))
      ;; These vars are buffer local.
      (el/setq buffer-file-name path)
      (el/setq buffer-file-truename filename) ; Might be correct, should be result of files/file-truename.
      (el/setq buffer-saved-size (count contents)))
    (doseq [f (data/symbol-value 'after-insert-file-functions)]
      (editfns/goto-char 1)
      (eval/funcall f (count contents)))
    (editfns/goto-char point)
    (list path (count contents))))

(defun file-name-nondirectory (filename)
  "Return file name FILENAME sans its directory.
  For example, in a Unix-syntax file name,
  this is everything after the last slash,
  or the entire name if it contains no slash."
  (el/check-type 'stringp filename)
  (.getName (io/file filename)))

(defun substitute-in-file-name (filename)
  "Substitute environment variables referred to in FILENAME.
  `$FOO' where FOO is an environment variable name means to substitute
  the value of that variable.  The variable name should be terminated
  with a character not a letter, digit or underscore; otherwise, enclose
  the entire variable name in braces.

  If `/~' appears, all of FILENAME through that `/' is discarded.
  If `//' appears, everything up to and including the first of
  those `/' is discarded."
  (let [filename (-> filename
                     (s/replace #".+(~/.+)" "$1")
                     (s/replace #".+/(/.+)" "$1"))
        vars (re-seq #"\$(\w+|\{.+\})" filename)]
    (reduce #(s/replace %1 (first %2) (System/getenv (second %2))) filename vars)))

(defun file-directory-p (filename)
  "Return t if FILENAME names an existing directory.
  Symbolic links to directories count as directories.
  See `file-symlink-p' to distinguish symlinks."
  (el/check-type 'stringp filename)
  (.isDirectory (io/file filename)))

(defun set-buffer-auto-saved ()
  "Mark current buffer as auto-saved with its current text.
  No auto-save file will be written until the buffer changes again."
  )

(defun set-file-times (filename &optional timestamp)
  "Set times of file FILENAME to TIMESTAMP.
  Set both access and modification times.
  Return t on success, else nil.
  Use the current time if TIMESTAMP is nil.  TIMESTAMP is in the format of
  `current-time'."
  )

(defun set-file-modes (filename mode)
  "Set mode bits of file named FILENAME to MODE (an integer).
  Only the 12 low bits of MODE are used.

  Interactively, mode bits are read by `read-file-modes', which accepts
  symbolic notation, like the `chmod' command from GNU Coreutils."
  )

(defun file-accessible-directory-p (filename)
  "Return t if file FILENAME names a directory you can open.
  For the value to be t, FILENAME must specify the name of a directory as a file,
  and the directory must allow you to open files in it.  In order to use a
  directory as a buffer's current directory, this predicate must return true.
  A directory name spec may be given instead; then the value is t
  if the directory so specified exists and really is a readable and
  searchable directory."
  (el/check-type 'stringp filename)
  (file-directory-p filename))

(defun file-name-as-directory (file)
  "Return a string representing the file name FILE interpreted as a directory.
  This operation exists because a directory is also a file, but its name as
  a directory is different from its name as a file.
  The result can be used as the value of `default-directory'
  or passed as second argument to `expand-file-name'.
  For a Unix-syntax file name, just appends a slash."
  (el/check-type 'stringp file)
  (if (re-find #"/$" file)
    file
    (str file "/")))

(defun copy-file (file newname &optional ok-if-already-exists keep-time preserve-uid-gid preserve-selinux-context)
  "Copy FILE to NEWNAME.  Both args must be strings.
  If NEWNAME names a directory, copy FILE there.

  This function always sets the file modes of the output file to match
  the input file.

  The optional third argument OK-IF-ALREADY-EXISTS specifies what to do
  if file NEWNAME already exists.  If OK-IF-ALREADY-EXISTS is nil, we
  signal a `file-already-exists' error without overwriting.  If
  OK-IF-ALREADY-EXISTS is a number, we request confirmation from the user
  about overwriting; this is what happens in interactive use with M-x.
  Any other value for OK-IF-ALREADY-EXISTS means to overwrite the
  existing file.

  Fourth arg KEEP-TIME non-nil means give the output file the same
  last-modified time as the old one.  (This works on only some systems.)

  A prefix arg makes KEEP-TIME non-nil.

  If PRESERVE-UID-GID is non-nil, we try to transfer the
  uid and gid of FILE to NEWNAME.

  If PRESERVE-SELINUX-CONTEXT is non-nil and SELinux is enabled
  on the system, we copy the SELinux context of FILE to NEWNAME."
  )

(defun file-regular-p (filename)
  "Return t if FILENAME names a regular file.
  This is the sort of file that holds an ordinary stream of data bytes.
  Symbolic links to regular files count as regular files.
  See `file-symlink-p' to distinguish symlinks."
  (el/check-type 'stringp filename)
  (Files/isRegularFile (.toPath (io/file filename)) (make-array LinkOption 0)))

(defun file-name-directory (filename)
  "Return the directory component in file name FILENAME.
  Return nil if FILENAME does not include a directory.
  Otherwise return a directory name.
  Given a Unix syntax file name, returns a string ending in slash."
  (el/check-type 'stringp filename)
  (if (re-find #"/$" filename)
    filename
    (when-let [parent (.getParent (io/file filename))]
      (file-name-as-directory parent))))

(defun file-symlink-p (filename)
  "Return non-nil if file FILENAME is the name of a symbolic link.
  The value is the link target, as a string.
  Otherwise it returns nil.

  This function returns t when given the name of a symlink that
  points to a nonexistent file."
  (el/check-type 'stringp filename)
  (let [path (.toPath (io/file filename))]
    (when (Files/isSymbolicLink path)
      (try
        (str (.toRealPath path (make-array LinkOption 0)))
        (catch NoSuchFileException _
          true)))))

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

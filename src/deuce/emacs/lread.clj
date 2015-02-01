(ns deuce.emacs.lread
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.walk :as w]
            [fipp.edn :as fipp]
            [lanterna.screen :as sc]
            [lanterna.common]
            [lanterna.constants]
            [taoensso.timbre :as timbre]
            [deuce.emacs-lisp :as el]
            [deuce.emacs-lisp.cons :refer [car cdr] :as cons]
            [deuce.emacs-lisp.globals :as globals]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.buffer :as buffer]
            [deuce.emacs.data :as data]
            [deuce.emacs.editfns :as editfns]
            [deuce.emacs.eval :as eval]
            [deuce.emacs.fileio :as fileio]
            [deuce.emacs.terminal :as terminal]
            [deuce.emacs.window :as window]
            [deuce.emacs-lisp.parser :as parser])
  (:refer-clojure :exclude [read intern load])
  (:import [java.io FileNotFoundException]
           [java.net URL]
           [com.googlecode.lanterna.input Key]
           [clojure.lang Symbol Compiler]))

(defvar old-style-backquotes nil
  "Set to non-nil when `read' encounters an old-style backquote.")

(defvar values nil
  "List of values of all expressions which were read, evaluated and printed.
  Order is reverse chronological.")

(defvar force-load-messages nil
  "Non-nil means force printing messages when loading Lisp files.
  This overrides the value of the NOMESSAGE argument to `load'.")

(defvar read-with-symbol-positions nil
  "If non-nil, add position of read symbols to `read-symbol-positions-list'.

  If this variable is a buffer, then only forms read from that buffer
  will be added to `read-symbol-positions-list'.
  If this variable is t, then all read forms will be added.
  The effect of all other values other than nil are not currently
  defined, although they may be in the future.

  The positions are relative to the last call to `read' or
  `read-from-string'.  It is probably a bad idea to set this variable at
  the toplevel; bind it instead.")

(defvar read-symbol-positions-list nil
  "A list mapping read symbols to their positions.
  This variable is modified during calls to `read' or
  `read-from-string', but only when `read-with-symbol-positions' is
  non-nil.

  Each element of the list looks like (SYMBOL . CHAR-POSITION), where
  CHAR-POSITION is an integer giving the offset of that occurrence of the
  symbol from the position where `read' or `read-from-string' started.

  Note that a symbol will appear multiple times in this list, if it was
  read multiple times.  The list is in the same order as the symbols
  were read in.")

(defvar load-path (alloc/list "")
  "*List of directories to search for files to load.
  Each element is a string (directory name) or nil (try default directory).
  Initialized based on EMACSLOADPATH environment variable, if any,
  otherwise to default specified by file `epaths.h' when Emacs was built.")

(defvar load-history nil
  "Alist mapping loaded file names to symbols and features.
  Each alist element should be a list (FILE-NAME ENTRIES...), where
  FILE-NAME is the name of a file that has been loaded into Emacs.
  The file name is absolute and true (i.e. it doesn't contain symlinks).
  As an exception, one of the alist elements may have FILE-NAME nil,
  for symbols and features not associated with any file.

  The remaining ENTRIES in the alist element describe the functions and
  variables defined in that file, the features provided, and the
  features required.  Each entry has the form `(provide . FEATURE)',
  `(require . FEATURE)', `(defun . FUNCTION)', `(autoload . SYMBOL)',
  `(defface . SYMBOL)', or `(t . SYMBOL)'.  Entries like `(t . SYMBOL)'
  may precede a `(defun . FUNCTION)' entry, and means that SYMBOL was an
  autoload before this file redefined it as a function.  In addition,
  entries may also be single symbols, which means that SYMBOL was
  defined by `defvar' or `defconst'.

  During preloading, the file name recorded is relative to the main Lisp
  directory.  These file names are converted to absolute at startup.")

(defvar user-init-file nil
  "File name, including directory, of user's initialization file.
  If the file loaded had extension `.elc', and the corresponding source file
  exists, this variable contains the name of source file, suitable for use
  by functions like `custom-save-all' which edit the init file.
  While Emacs loads and evaluates the init file, value is the real name
  of the file, regardless of whether or not it has the `.elc' extension.")

(defvar preloaded-file-list nil
  "List of files that were preloaded (when dumping Emacs).")

(defvar bytecomp-version-regexp nil
  "Regular expression matching safe to load compiled Lisp files.
  When Emacs loads a compiled Lisp file, it reads the first 512 bytes
  from the file, and matches them against this regular expression.
  When the regular expression matches, the file is considered to be safe
  to load.  See also `load-dangerous-libraries'.")

(defvar load-file-name nil
  "Full name of file being loaded by `load'.")

(defvar load-suffixes (alloc/list ".class" ".el")
  "List of suffixes for (compiled or source) Emacs Lisp files.
  This list should not include the empty string.
  `load' and related functions try to append these suffixes, in order,
  to the specified file name if a Lisp suffix is allowed or required.")

(defvar load-convert-to-unibyte nil
  "Non-nil means `read' converts strings to unibyte whenever possible.
  This is normally bound by `load' and `eval-buffer' to control `read',
  and is not meant for users to change.")

(defvar load-file-rep-suffixes (alloc/list "")
  "List of suffixes that indicate representations of the same file.
  This list should normally start with the empty string.

  Enabling Auto Compression mode appends the suffixes in
  `jka-compr-load-suffixes' to this list and disabling Auto Compression
  mode removes them again.  `load' and related functions use this list to
  determine whether they should look for compressed versions of a file
  and, if so, which suffixes they should try to append to the file name
  in order to do so.  However, if you want to customize which suffixes
  the loading functions recognize as compression suffixes, you should
  customize `jka-compr-load-suffixes' rather than the present variable.")

(defvar load-source-file-function nil
  "Function called in `load' for loading an Emacs Lisp source file.
  This function is for doing code conversion before reading the source file.
  If nil, loading is done without any code conversion.
  Arguments are FULLNAME, FILE, NOERROR, NOMESSAGE, where
   FULLNAME is the full name of FILE.
  See `load' for the meaning of the remaining arguments.")

(defvar load-read-function nil
  "Function used by `load' and `eval-region' for reading expressions.
  The default is nil, which means use the function `read'.")

(defvar obarray nil
  "Symbol table for use by `intern' and `read'.
  It is a vector whose length ought to be prime for best results.
  The vector's contents don't make sense if examined from Lisp programs;
  to find all the symbols in an obarray, use `mapatoms'.")

(defvar read-circle nil
  "Non-nil means read recursive structures using #N= and #N# syntax.")

(defvar eval-buffer-list nil
  "List of buffers being read from by calls to `eval-buffer' and `eval-region'.")

(defvar load-force-doc-strings nil
  "Non-nil means `load' should force-load all dynamic doc strings.
  This is useful when the file being loaded is a temporary copy.")

(defvar byte-boolean-vars nil
  "List of all DEFVAR_BOOL variables, used by the byte code optimizer.")

(defvar after-load-alist nil
  "An alist of expressions to be evalled when particular files are loaded.
  Each element looks like (REGEXP-OR-FEATURE FORMS...).

  REGEXP-OR-FEATURE is either a regular expression to match file names, or
  a symbol (a feature name).

  When `load' is run and the file-name argument matches an element's
  REGEXP-OR-FEATURE, or when `provide' is run and provides the symbol
  REGEXP-OR-FEATURE, the FORMS in the element are executed.

  An error in FORMS does not undo the load, but does prevent execution of
  the rest of the FORMS.")

(defvar load-in-progress nil
  "Non-nil if inside of `load'.")

(defvar load-dangerous-libraries nil
  "Non-nil means load dangerous compiled Lisp files.
  Some versions of XEmacs use different byte codes than Emacs.  These
  incompatible byte codes can make Emacs crash when it tries to execute
  them.")

(defvar current-load-list nil
  "Used for internal purposes by `load'.")

(defvar source-directory nil
  "Directory in which Emacs sources were found when Emacs was built.
  You cannot count on them to still be there!")

(defvar lexical-binding nil
  "Whether to use lexical binding when evaluating code.
  Non-nil means that the code in the current buffer should be evaluated
  with lexical binding.
  This variable is automatically set from the file variables of an
  interpreted Lisp file read using `load'.  Unlike other file local
  variables, this must be set in the first line of a file.")

(defvar standard-input nil
  "Stream for read to get input from.
  See documentation of `read' for possible values.")

(defn ^:private echo [message]
  ;; Emacs uses 2 echo areas and switches between them.
  (let [echo-area (buffer/get-buffer-create " *Echo Area 0*")]
    (if (seq message)
      (binding [buffer/*current-buffer* echo-area]
        (buffer/erase-buffer)
        (editfns/insert message))
      (binding [buffer/*current-buffer* echo-area]
        (buffer/erase-buffer)))
    (window/set-window-buffer (window/minibuffer-window) echo-area)))

(defn ^:private read-event-internal [prompt inherit-input-method seconds]
  (when prompt (echo prompt))
  ((ns-resolve 'deuce.emacs.keyboard 'read-char) nil nil nil nil
   (+ (System/currentTimeMillis)
      (* 1000 seconds))))

(defn ^:private key-to-integer [^Key k]
  (parser/event-convert-list-internal
   (remove nil? [(when (.isAltPressed k) 'meta)
                 (when (.isCtrlPressed k) 'control)])
   (.getCharacter k)))

;; This should maybe use the input-decode-map?
(def ^:private lanterna-to-emacs-event {:enter "return"
                                        :page-down "next"
                                        :page-up "prior"
                                        :reverse-tab "S-iso-lefttab"})

(def ^:private lanterna-valid-chars {:escape (Key. \)
                                     :backspace (Key. \)
                                     :enter (Key. \return)})

(defun read-event (&optional prompt inherit-input-method seconds)
  "Read an event object from the input stream.
  If the optional argument PROMPT is non-nil, display that as a prompt.
  If the optional argument INHERIT-INPUT-METHOD is non-nil and some
  input method is turned on in the current buffer, that input method
  is used for reading a character.
  If the optional argument SECONDS is non-nil, it should be a number
  specifying the maximum number of seconds to wait for input.  If no
  input arrives in that time, return nil.  SECONDS may be a
  floating-point value."
  (let [^Key k (read-event-internal prompt inherit-input-method seconds)
        kind (lanterna.constants/key-codes (.getKind k))]
    (if (= kind :normal)
      (key-to-integer k)
      (symbol (str (when (.isCtrlPressed k) "C-")
                   (when (.isAltPressed k) "M-")
                   (lanterna-to-emacs-event kind (name kind)))))))

(defun read-char-exclusive (&optional prompt inherit-input-method seconds)
  "Read a character from the command input (keyboard or macro).
  It is returned as a number.  Non-character events are ignored.
  If the character has modifiers, they are resolved and reflected to the
  character code if possible (e.g. C-SPC -> 0).

  If the optional argument PROMPT is non-nil, display that as a prompt.
  If the optional argument INHERIT-INPUT-METHOD is non-nil and some
  input method is turned on in the current buffer, that input method
  is used for reading a character.
  If the optional argument SECONDS is non-nil, it should be a number
  specifying the maximum number of seconds to wait for input.  If no
  input arrives in that time, return nil.  SECONDS may be a
  floating-point value."
  ;; Non-normal keys with modifiers, like Shift-Arrow-Up etc.
  ;; comes as a string of escape codes probably not dealt with by Lanterna.
  ;; See com.googlecode.lanterna.input.CommonProfile
  ;; We might want to write a specific EmacsProfile.
  (let [k (read-event-internal prompt inherit-input-method seconds)
        kind (lanterna.constants/key-codes (.getKind k))
        k (lanterna-valid-chars kind k)
        kind (lanterna.constants/key-codes (.getKind k))]
    (if (or (= :normal kind) (Character/isISOControl (.getCharacter k)))
      (key-to-integer k)
      (el/throw* 'error "Non-character input-event"))))

(defun read (&optional stream)
  "Read one Lisp expression as text from STREAM, return as Lisp object.
  If STREAM is nil, use the value of `standard-input' (which see).
  STREAM or the value of `standard-input' may be:
   a buffer (read from point and advance it)
   a marker (read from where it points and advance it)
   a function (call it with no arguments for each character,
       call it with a char as argument to push a char back)
   a string (takes text from string, starting at the beginning)
   t (read text line using minibuffer and use it, or read from
      standard input in batch mode)."
  (let [stream (or stream (data/symbol-value 'standard-input))
        stream (if (data/bufferp stream)
                 (editfns/buffer-substring (editfns/point) (inc (editfns/buffer-size)))
                 stream)]
    (first (parser/parse stream))))

(defun read-char (&optional prompt inherit-input-method seconds)
  "Read a character from the command input (keyboard or macro).
  It is returned as a number.
  If the character has modifiers, they are resolved and reflected to the
  character code if possible (e.g. C-SPC -> 0).

  If the user generates an event which is not a character (i.e. a mouse
  click or function key event), `read-char' signals an error.  As an
  exception, switch-frame events are put off until non-character events
  can be read.
  If you want to read non-character events, or ignore them, call
  `read-event' or `read-char-exclusive' instead.

  If the optional argument PROMPT is non-nil, display that as a prompt.
  If the optional argument INHERIT-INPUT-METHOD is non-nil and some
  input method is turned on in the current buffer, that input method
  is used for reading a character.
  If the optional argument SECONDS is non-nil, it should be a number
  specifying the maximum number of seconds to wait for input.  If no
  input arrives in that time, return nil.  SECONDS may be a
  floating-point value."
  (read-char-exclusive prompt inherit-input-method seconds))

(defun eval-buffer (&optional buffer printflag filename unibyte do-allow-print)
  "Execute the current buffer as Lisp code.
  When called from a Lisp program (i.e., not interactively), this
  function accepts up to five optional arguments:
  BUFFER is the buffer to evaluate (nil means use current buffer).
  PRINTFLAG controls printing of output:
   A value of nil means discard it; anything else is stream for print.
  FILENAME specifies the file name to use for `load-history'.
  UNIBYTE, if non-nil, specifies `load-convert-to-unibyte' for this
   invocation.
  DO-ALLOW-PRINT, if non-nil, specifies that `print' and related
   functions should work normally even if PRINTFLAG is nil.

  This function preserves the position of point."
  (interactive))

(defun read-from-string (string &optional start end)
  "Read one Lisp expression which is represented as text by STRING.
  Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).
  FINAL-STRING-INDEX is an integer giving the position of the next
   remaining character in STRING.
  START and END optionally delimit a substring of STRING from which to read;
   they default to 0 and (length STRING) respectively."
  (let [string (subs string (or start 0) (or end (count string)))]
    (try
      (if (= \{ (first string))
        (alloc/cons (read-string string) (count string))
        (parser/parse-internal string))
      (catch Exception e
        (parser/parse-internal string)))))

(defun eval-region (start end &optional printflag read-function)
  "Execute the region as Lisp code.
  When called from programs, expects two arguments,
  giving starting and ending indices in the current buffer
  of the text to be executed.
  Programs can pass third argument PRINTFLAG which controls output:
  A value of nil means discard it; anything else is stream for printing it.
  Also the fourth argument READ-FUNCTION, if non-nil, is used
  instead of `read' to read each expression.  It gets one argument
  which is the input stream for reading characters.

  This function does not move point."
  (interactive "r"))

(defun intern (string &optional obarray)
  "Return the canonical symbol whose name is STRING.
  If there is none, one is created by this function and returned.
  A second optional argument specifies the obarray to use;
  it defaults to the value of `obarray'."
  (symbol nil string))

(defun get-load-suffixes ()
  "Return the suffixes that `load' should try if a suffix is required.
  This uses the variables `load-suffixes' and `load-file-rep-suffixes'."
  (apply alloc/list (remove empty? (concat globals/load-file-rep-suffixes globals/load-suffixes))))

(def ^:private ^:dynamic *pretty-clojure* true)

(extend-protocol fipp/IPretty
  java.lang.Object
  (-pretty [x ctx]
    (binding [*print-dup* true]
      [:text (pr-str x)])))

(defn ^:private write-clojure [el clj]
  (io/make-parents clj)
  (spit clj
        (with-out-str
          (doseq [form (concat '[(ns deuce.emacs (:refer-clojure :only []))]
                               el)]
            (if *pretty-clojure*
              (fipp/pprint form)
              (pr form))
            (println)))))

(def ^:private access {0 fileio/file-exists-p
                       1 fileio/file-readable-p
                       2 fileio/file-writable-p
                       3 fileio/file-executable-p})

(defn ^:private locate-file [filename path suffixes predicate]
  (let [predicate (or predicate (el/fun 'file-readable-p))
        predicate (access predicate predicate)]
    (->> (for [l path
               :let [file (str l "/" filename)
                     find-resource #(let [url (str (s/replace file  #"^/*" "") %)]
                                      (or (io/resource (s/replace url "-" "_"))
                                          (io/resource url)))
                     find-file #(let [f (io/file (str file %))]
                                  (and (.exists f) (predicate (.getAbsolutePath f))
                                       (.toURL f)))]]
           [l (some identity (map (some-fn find-resource find-file) (or suffixes [""])))])
         (filter (comp identity second))
         (remove #(when (= "file" (.getProtocol (second %)))
                    (.isDirectory (io/file (second %)))))
         first)))

(def ^:private ^:dynamic loads-in-progress #{})

(defn ^:private internal-path [path file]
  (s/replace (str (when (seq path) (str path "/")) file) #"^/*" ""))

(defun load (file &optional noerror nomessage nosuffix must-suffix)
  "Execute a file of Lisp code named FILE.
  First try FILE with `.elc' appended, then try with `.el',
  then try FILE unmodified (the exact suffixes in the exact order are
  determined by `load-suffixes').  Environment variable references in
  FILE are replaced with their values by calling `substitute-in-file-name'.
  This function searches the directories in `load-path'.

  If optional second arg NOERROR is non-nil,
  report no error if FILE doesn't exist.
  Print messages at start and end of loading unless
  optional third arg NOMESSAGE is non-nil (but `force-load-messages'
  overrides that).
  If optional fourth arg NOSUFFIX is non-nil, don't try adding
  suffixes `.elc' or `.el' to the specified name FILE.
  If optional fifth arg MUST-SUFFIX is non-nil, insist on
  the suffix `.elc' or `.el'; don't accept just FILE unless
  it ends in one of those suffixes or includes a directory name.

  If this function fails to find a file, it may look for different
  representations of that file before trying another file.
  It does so by adding the non-empty suffixes in `load-file-rep-suffixes'
  to the file name.  Emacs uses this feature mainly to find compressed
  versions of files when Auto Compression mode is enabled.

  The exact suffixes that this function tries out, in the exact order,
  are given by the value of the variable `load-file-rep-suffixes' if
  NOSUFFIX is non-nil and by the return value of the function
  `get-load-suffixes' if MUST-SUFFIX is non-nil.  If both NOSUFFIX and
  MUST-SUFFIX are nil, this function first tries out the latter suffixes
  and then the former.

  Loading a file records its definitions, and its `provide' and
  `require' calls, in an element of `load-history' whose
  car is the file name loaded.  See `load-history'.

  While the file is in the process of being loaded, the variable
  `load-in-progress' is non-nil and the variable `load-file-name'
  is bound to the file's name.

  Return t if the file exists and loads successfully."
  ;; Need to deal with -*- lexical-binding: t -*-
  (if (loads-in-progress file)
    true ;; not really correct
    (binding [loads-in-progress (conj loads-in-progress file)]
      (try
        (let [[path url] (locate-file file (data/symbol-value 'load-path)
                                      (when-not nosuffix '("" ".el")) nil)
              el-extension? (re-find #".el$" file)]
          (if-not url
            (el/throw* 'file-error (list "Cannot open load file" file)))
          (when-not nomessage
            (editfns/message "Loading %s%s..." file (if el-extension? " (source)" "")))
          (binding [globals/load-file-name (.getFile url)
                    globals/load-in-progress true]
            (let [file (internal-path path (s/replace file  #".el$" ""))
                  clj-file (str (s/replace file "-" "_") ".clj")
                  clj-name (symbol (s/replace file "/" "."))
                  last-modified #(if % (.getLastModified (.openConnection ^URL %)) -1)
                  load-raw-clj #(if-let [r (io/resource clj-file)]
                                  (with-open [r (io/reader r)]
                                    (Compiler/load r clj-file (.getName (io/file clj-file))))
                                  (throw (FileNotFoundException. "no clj file")))]
              (try
                (when (> (last-modified url) (last-modified (io/resource clj-file)))
                  (throw (FileNotFoundException. "out of date")))
                (if el-extension?
                  (load-raw-clj)
                  (c/require clj-name))
                (catch FileNotFoundException _
                  (binding [*compile-path* (or (some-> 'deuce.main/*emacs-compile-path* resolve deref)
                                               *compile-path*)]
                    (with-open [in (io/input-stream url)]
                      (let [el (parser/parse in)
                            clj-file (io/file *compile-path* clj-file)]
                        (write-clojure (map el/el->clj el) clj-file)
                        (if el-extension?
                          (load-raw-clj)
                          (binding [*compile-files* true]
                            (require clj-name))))))))
              true)))
        (catch Exception e
          (when-not noerror
            (throw e)))))))

(defun mapatoms (function &optional obarray)
  "Call FUNCTION on every symbol in OBARRAY.
  OBARRAY defaults to the value of `obarray'."
  )

(defun locate-file-internal (filename path &optional suffixes predicate)
  "Search for FILENAME through PATH.
  Returns the file's name in absolute form, or nil if not found.
  If SUFFIXES is non-nil, it should be a list of suffixes to append to
  file name when searching.
  If non-nil, PREDICATE is used instead of `file-readable-p'.
  PREDICATE can also be an integer to pass to the access(2) function,
  in which case file-name-handlers are ignored.
  This function will normally skip directories, so if you want it to find
  directories, make sure the PREDICATE function returns `dir-ok' for them."
  (let [[dir file] (locate-file filename path suffixes predicate)]
    (when file
      (internal-path dir (str (io/file (.getParent (io/file filename))
                                       (.getName (io/file (.getFile file)))))))))

(defun unintern (name obarray)
  "Delete the symbol named NAME, if any, from OBARRAY.
  The value is t if a symbol was found and deleted, nil otherwise.
  NAME may be a string or a symbol.  If it is a symbol, that symbol
  is deleted, if it belongs to OBARRAY--no other symbol is deleted.
  OBARRAY defaults to the value of the variable `obarray'."
  )

(defun get-file-char ()
  "Don't use this yourself."
  )

(defun intern-soft (name &optional obarray)
  "Return the canonical symbol named NAME, or nil if none exists.
  NAME may be a string or a symbol.  If it is a symbol, that exact
  symbol is searched for.
  A second optional argument specifies the obarray to use;
  it defaults to the value of `obarray'."
  (intern name))

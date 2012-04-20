(ns
 deuce.emacs.lread
 (use [deuce.emacs-lisp :only (defun t)])
 (require [clojure.core :as c])
 (:refer-clojure :exclude [read intern load])
 (import [java.io Reader PushbackReader]))

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
  )

(defun read-char-exclusive (&optional prompt inherit-input-method seconds)
  "Read a character from the command input (keyboard or macro).
  It is returned as a number.  Non-character events are ignored.
  If the character has modifiers, they are resolved and reflected to the
  character code if possible (e.g. C-SPC -> 0)."
  )

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
  (condp some [stream]
    string? (read-string stream)
    (partial
     contains? #{t nil}) (c/read)
    (partial
     instance? Reader) (c/read (if (instance? PushbackReader stream) stream
                                   (PushbackReader. stream)))
    (assert false stream)))

(defun read-char (&optional prompt inherit-input-method seconds)
  "Read a character from the command input (keyboard or macro).
  It is returned as a number.
  If the character has modifiers, they are resolved and reflected to the
  character code if possible (e.g. C-SPC -> 0)."
  )

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
   functions should work normally even if PRINTFLAG is nil."
  )

(defun read-from-string (string &optional start end)
  "Read one Lisp expression which is represented as text by STRING.
  Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).
  START and END optionally delimit a substring of STRING from which to read;
   they default to 0 and (length STRING) respectively."
  )

(defun eval-region (start end &optional printflag read-function)
  "Execute the region as Lisp code.
  When called from programs, expects two arguments,
  giving starting and ending indices in the current buffer
  of the text to be executed.
  Programs can pass third argument PRINTFLAG which controls output:
  A value of nil means discard it; anything else is stream for printing it.
  Also the fourth argument READ-FUNCTION, if non-nil, is used
  instead of `read' to read each expression.  It gets one argument
  which is the input stream for reading characters."
  )

(defun intern (string &optional obarray)
  "Return the canonical symbol whose name is STRING.
  If there is none, one is created by this function and returned.
  A second optional argument specifies the obarray to use;
  it defaults to the value of `obarray'."
  )

(defun get-load-suffixes ()
  "Return the suffixes that `load' should try if a suffix is required.
  This uses the variables `load-suffixes' and `load-file-rep-suffixes'."
  )

(defun load (file &optional noerror nomessage nosuffix must-suffix)
  "Execute a file of Lisp code named FILE.
  First try FILE with `.elc' appended, then try with `.el',
  then try FILE unmodified (the exact suffixes in the exact order are
  determined by `load-suffixes').  Environment variable references in
  FILE are replaced with their values by calling `substitute-in-file-name'.
  This function searches the directories in `load-path'."
  )

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
  in which case file-name-handlers are ignored."
  )

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
  )

(ns deuce.emacs.editfns
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c]
            [clojure.string :as s]
            [deuce.emacs.buffer :as buffer]
            [deuce.emacs.data :as data]
            [deuce.emacs-lisp :as el]
            [deuce.emacs-lisp.globals :as globals])
  (:import [java.net InetAddress]
           [java.text SimpleDateFormat]
           [java.util Date Calendar TimeZone List]
           [deuce.emacs.data Marker])
  (:refer-clojure :exclude [format]))

(defvar buffer-access-fontified-property nil
  "Property which (if non-nil) indicates text has been fontified.
  `buffer-substring' need not call the `buffer-access-fontify-functions'
  functions if all the text being accessed has this property.")

(defvar operating-system-release (System/getProperty "os.version")
  "The release of the operating system Emacs is running on.")

(defvar user-real-login-name (System/getProperty "user.name")
  "The user's name, based upon the real uid only.")

(defvar buffer-access-fontify-functions nil
  "List of functions called by `buffer-substring' to fontify if necessary.
  Each function is called with two arguments which specify the range
  of the buffer being accessed.")

(defvar user-login-name (System/getProperty "user.name")
  "The user's name, taken from environment variables if possible.")

(defvar system-name (.getHostName (InetAddress/getLocalHost))
  "The host name of the machine Emacs is running on.")

(defvar inhibit-field-text-motion nil
  "Non-nil means text motion commands don't notice fields.")

(defvar user-full-name nil
  "The full name of the user logged in.

  You can customize this variable.")

(defun byte-to-position (bytepos)
  "Return the character position for byte position BYTEPOS.
  If BYTEPOS is out of range, the value is nil."
  )

(defun field-string-no-properties (&optional pos)
  "Return the contents of the field around POS, without text properties.
  A field is a region of text with the same `field' property.
  If POS is nil, the value of point is used for POS."
  )

(defn ^:private emacs-time-to-date [[high low usec]]
  (Date. (+ (* (+ (bit-shift-left high 16) low) 1000) (/ usec 1000))))

(defn ^:private date-to-emacs-time [date]
  (let [now (.getTime date)
        seconds (int (/ now 1000))]
    (list (bit-shift-right seconds 16) (bit-and 0xffff seconds) (* 1000 (mod now 1000)))))

(declare buffer-string buffer-substring buffer-size point mark-marker goto-char
         point-max point-min insert eobp bobp char-before)

(defun decode-time (&optional specified-time)
  "Decode a time value as (SEC MINUTE HOUR DAY MONTH YEAR DOW DST ZONE).
  The optional SPECIFIED-TIME should be a list of (HIGH LOW . IGNORED),
  as from `current-time' and `file-attributes', or nil to use the
  current time.  The obsolete form (HIGH . LOW) is also still accepted.
  The list has the following nine members: SEC is an integer between 0
  and 60; SEC is 60 for a leap second, which only some operating systems
  support.  MINUTE is an integer between 0 and 59.  HOUR is an integer
  between 0 and 23.  DAY is an integer between 1 and 31.  MONTH is an
  integer between 1 and 12.  YEAR is an integer indicating the
  four-digit year.  DOW is the day of week, an integer between 0 and 6,
  where 0 is Sunday.  DST is t if daylight saving time is in effect,
  otherwise nil.  ZONE is an integer indicating the number of seconds
  east of Greenwich.  (Note that Common Lisp has different meanings for
  DOW and ZONE.)"
  )

(defun current-time ()
  "Return the current time, as the number of seconds since 1970-01-01 00:00:00.
  The time is returned as a list of three integers.  The first has the
  most significant 16 bits of the seconds, while the second has the
  least significant 16 bits.  The third integer gives the microsecond
  count.

  The microsecond count is zero on systems that do not provide
  resolution finer than a second."
  (date-to-emacs-time (Date.)))

(defun point-max-marker ()
  "Return a marker to the maximum permissible value of point in this buffer.
  This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
  is in effect, in which case it is less."
  (Marker. (buffer/current-buffer) (point-max)))

(defun preceding-char ()
  "Return the character preceding point, as a number.
  At the beginning of the buffer or accessible region, return 0."
  (or (char-before) 0))

(defun translate-region-internal (start end table)
  "Internal use only.
  From START to END, translate characters according to TABLE.
  TABLE is a string or a char-table; the Nth character in it is the
  mapping for the character with code N.
  It returns the number of characters changed."
  )

(defun insert-before-markers-and-inherit (&rest args)
  "Insert text at point, relocating markers and inheriting properties.
  Point and markers move forward to end up after the inserted text.

  If the current buffer is multibyte, unibyte strings are converted
  to multibyte for insertion (see `unibyte-char-to-multibyte').
  If the current buffer is unibyte, multibyte strings are converted
  to unibyte for insertion."
  )

(defun field-beginning (&optional pos escape-from-edge limit)
  "Return the beginning of the field surrounding POS.
  A field is a region of text with the same `field' property.
  If POS is nil, the value of point is used for POS.
  If ESCAPE-FROM-EDGE is non-nil and POS is at the beginning of its
  field, then the beginning of the *previous* field is returned.
  If LIMIT is non-nil, it is a buffer position; if the beginning of the field
  is before LIMIT, then LIMIT will be returned instead."
  )

(defun format (string &rest objects)
  "Format a string out of a format-string and arguments.
  The first argument is a format control string.
  The other arguments are substituted into it to make the result, a string.

  The format control string may contain %-sequences meaning to substitute
  the next available argument:

  %s means print a string argument.  Actually, prints any object, with `princ'.
  %d means print as number in decimal (%o octal, %x hex).
  %X is like %x, but uses upper case.
  %e means print a number in exponential notation.
  %f means print a number in decimal-point notation.
  %g means print a number in exponential notation
    or decimal-point notation, whichever uses fewer characters.
  %c means print a number as a single character.
  %S means print any object as an s-expression (using `prin1').

  The argument used for %d, %o, %x, %e, %f, %g or %c must be a number.
  Use %% to put a single % into the output.

  A %-sequence may contain optional flag, width, and precision
  specifiers, as follows:

    %<flags><width><precision>character

  where flags is [+ #-0]+, width is [0-9]+, and precision is .[0-9]+

  The + flag character inserts a + before any positive number, while a
  space inserts a space before any positive number; these flags only
  affect %d, %e, %f, and %g sequences, and the + flag takes precedence.
  The # flag means to use an alternate display form for %o, %x, %X, %e,
  %f, and %g sequences.  The - and 0 flags affect the width specifier,
  as described below.

  The width specifier supplies a lower limit for the length of the
  printed representation.  The padding, if any, normally goes on the
  left, but it goes on the right if the - flag is present.  The padding
  character is normally a space, but it is 0 if the 0 flag is present.
  The 0 flag is ignored if the - flag is present, or the format sequence
  is something other than %d, %e, %f, and %g.

  For %e, %f, and %g sequences, the number after the \".\" in the
  precision specifier says how many decimal places to show; if zero, the
  decimal point itself is omitted.  For %s and %S, the precision
  specifier truncates the string to the given width."
  (apply c/format (s/replace string "%S" "%s")
         (map #(cond
                (and (instance? Long %)
                     (<= Integer/MIN_VALUE % Integer/MAX_VALUE)) (int %)
                     (instance? List %) (seq %)
                     :else %) objects)))

(defun user-uid ()
  "Return the effective uid of Emacs.
  Value is an integer or a float, depending on the value."
  )

(defun set-time-zone-rule (tz)
  "Set the local time zone using TZ, a string specifying a time zone rule.
  If TZ is nil, use implementation-defined default time zone information.
  If TZ is t, use Universal Time.

  Instead of calling this function, you typically want (setenv \"TZ\" TZ).
  That changes both the environment of the Emacs process and the
  variable `process-environment', whereas `set-time-zone-rule' affects
  only the former."
  )

(defun insert-and-inherit (&rest args)
  "Insert the arguments at point, inheriting properties from adjoining text.
  Point and before-insertion markers move forward to end up
   after the inserted text.
  Any other markers at the point of insertion remain before the text.

  If the current buffer is multibyte, unibyte strings are converted
  to multibyte for insertion (see `unibyte-char-to-multibyte').
  If the current buffer is unibyte, multibyte strings are converted
  to unibyte for insertion."
  (insert args))

(defun user-real-login-name ()
  "Return the name of the user's real uid, as a string.
  This ignores the environment variables LOGNAME and USER, so it differs from
  `user-login-name' when running under `su'."
  globals/user-real-login-name)

(defun emacs-pid ()
  "Return the process ID of Emacs, as an integer."
  )

(defun point-max ()
  "Return the maximum permissible value of point in the current buffer.
  This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
  is in effect, in which case it is less."
  (inc (buffer-size)))

(defun char-equal (c1 c2)
  "Return t if two characters match, optionally ignoring case.
  Both arguments must be characters (i.e. integers).
  Case is ignored if `case-fold-search' is non-nil in the current buffer."
  (= (s/lower-case c1) (s/lower-case c2)))

(defun encode-time (second minute hour day month year &optional zone)
  "Convert SECOND, MINUTE, HOUR, DAY, MONTH, YEAR and ZONE to internal time.
  This is the reverse operation of `decode-time', which see.
  ZONE defaults to the current time zone rule.  This can
  be a string or t (as from `set-time-zone-rule'), or it can be a list
  (as from `current-time-zone') or an integer (as from `decode-time')
  applied without consideration for daylight saving time.

  You can pass more than 7 arguments; then the first six arguments
  are used as SECOND through YEAR, and the *last* argument is used as ZONE.
  The intervening arguments are ignored.
  This feature lets (apply 'encode-time (decode-time ...)) work.

  Out-of-range values for SECOND, MINUTE, HOUR, DAY, or MONTH are allowed;
  for example, a DAY of 0 means the day preceding the given month.
  Year numbers less than 100 are treated just like other year numbers.
  If you want them to stand for years in this century, you must do that yourself.

  Years before 1970 are not guaranteed to work.  On some systems,
  year values as low as 1901 do work."
  )

(defun char-after (&optional pos)
  "Return character in current buffer at position POS.
  POS is an integer or a marker and defaults to point.
  If POS is out of range, the value is nil."
  (let [pos (or pos (point))]
      (when  (< -1 pos (buffer-size))
        (.charAt (buffer-string) pos))))

(defun gap-size ()
  "Return the size of the current buffer's gap.
  See also `gap-position'."
  )

(defun insert-buffer-substring (buffer &optional start end)
  "Insert before point a substring of the contents of BUFFER.
  BUFFER may be a buffer or a buffer name.
  Arguments START and END are character positions specifying the substring.
  They default to the values of (point-min) and (point-max) in BUFFER."
  )

(defun point-min-marker ()
  "Return a marker to the minimum permissible value of point in this buffer.
  This is the beginning, unless narrowing (a buffer restriction) is in effect."
  (Marker. (buffer/current-buffer) (point-min)))

(defun string-to-char (string)
  "Return the first character in STRING."
  (first string))

(defun point-marker ()
  "Return value of point, as a marker object."
  (Marker. (buffer/current-buffer) (point)))

(defun gap-position ()
  "Return the position of the gap, in the current buffer.
  See also `gap-size'."
  )

(defun eolp ()
  "Return t if point is at the end of a line.
  `End of a line' includes point being at the end of the buffer."
  (or (eobp) (= \newline (char-after))))

(defun line-end-position (&optional n)
  "Return the character position of the last character on the current line.
  With argument N not nil or 1, move forward N - 1 lines first.
  If scan reaches end of buffer, return that position.

  The returned position is of the last character in the logical order,
  i.e. the character whose buffer position is the largest one.

  This function constrains the returned position to the current field
  unless that would be on a different line than the original,
  unconstrained result.  If N is nil or 1, and a rear-sticky field ends
  at point, the scan stops as soon as it starts.  To ignore field
  boundaries bind `inhibit-field-text-motion' to t.

  This function does not move point."
  )

(defun narrow-to-region (start end)
  "Restrict editing in this buffer to the current region.
  The rest of the text becomes temporarily invisible and untouchable
  but is not deleted; if you save the buffer in a file, the invisible
  text is included in the file.  C-x n w makes all visible again.
  See also `save-restriction'.

  When calling from a program, pass two arguments; positions (integers
  or markers) bounding the text that should remain visible."
  )

(defun get-internal-run-time ()
  "Return the current run time used by Emacs.
  The time is returned as a list of three integers.  The first has the
  most significant 16 bits of the seconds, while the second has the
  least significant 16 bits.  The third integer gives the microsecond
  count.

  On systems that can't determine the run time, `get-internal-run-time'
  does the same thing as `current-time'.  The microsecond count is zero
  on systems that do not provide resolution finer than a second."
  (current-time))

(defun point-min ()
  "Return the minimum permissible value of point in the current buffer.
  This is 1, unless narrowing (a buffer restriction) is in effect."
  1)

(defun widen ()
  "Remove restrictions (narrowing) from current buffer.
  This allows the buffer's full text to be seen and edited."
  )

(defun subst-char-in-region (start end fromchar tochar &optional noundo)
  "From START to END, replace FROMCHAR with TOCHAR each time it occurs.
  If optional arg NOUNDO is non-nil, don't record this change for undo
  and don't mark the buffer as really changed.
  Both characters must have the same length of multi-byte form."
  (let [text (.own-text (buffer/current-buffer))]
    (.replace (.beg text)
              start end
              (s/replace (buffer-substring start end) fromchar tochar))
    (reset! (.modiff text) (System/currentTimeMillis))
    nil))

(defun bolp ()
  "Return t if point is at the beginning of a line."
  (or (bobp) (= \newline (char-before))))

(defun position-bytes (position)
  "Return the byte position for character position POSITION.
  If POSITION is out of range, the value is nil."
  )

(defun point ()
  "Return value of point, as an integer.
  Beginning of buffer is position (point-min)."
  @(.pt (buffer/current-buffer)))

(defun field-string (&optional pos)
  "Return the contents of the field surrounding POS as a string.
  A field is a region of text with the same `field' property.
  If POS is nil, the value of point is used for POS."
  )

(defun region-beginning ()
  "Return the integer value of point or mark, whichever is smaller."
  (min (.charpos (mark-marker)) (point)))

(defun line-beginning-position (&optional n)
  "Return the character position of the first character on the current line.
  With argument N not nil or 1, move forward N - 1 lines first.
  If scan reaches end of buffer, return that position.

  The returned position is of the first character in the logical order,
  i.e. the one that has the smallest character position.

  This function constrains the returned position to the current field
  unless that would be on a different line than the original,
  unconstrained result.  If N is nil or 1, and a front-sticky field
  starts at point, the scan stops as soon as it starts.  To ignore field
  boundaries bind `inhibit-field-text-motion' to t.

  This function does not move point."
  )

(defun following-char ()
  "Return the character following point, as a number.
  At the end of the buffer or accessible region, return 0."
  (or (char-after) 0))

(defun eobp ()
  "Return t if point is at the end of the buffer.
  If the buffer is narrowed, this means the end of the narrowed part."
  (= (point) (point-max)))

(defun buffer-substring-no-properties (start end)
  "Return the characters of part of the buffer, without the text properties.
  The two arguments START and END are character positions;
  they can be in either order."
  )

(defun user-real-uid ()
  "Return the real uid of Emacs.
  Value is an integer or a float, depending on the value."
  )

(defun field-end (&optional pos escape-from-edge limit)
  "Return the end of the field surrounding POS.
  A field is a region of text with the same `field' property.
  If POS is nil, the value of point is used for POS.
  If ESCAPE-FROM-EDGE is non-nil and POS is at the end of its field,
  then the end of the *following* field is returned.
  If LIMIT is non-nil, it is a buffer position; if the end of the field
  is after LIMIT, then LIMIT will be returned instead."
  )

(defun user-login-name (&optional uid)
  "Return the name under which the user logged in, as a string.
  This is based on the effective uid, not the real uid.
  Also, if the environment variables LOGNAME or USER are set,
  that determines the value of this function.

  If optional argument UID is an integer or a float, return the login name
  of the user with that uid, or nil if there is no such user."
  globals/user-login-name)

(defun bobp ()
  "Return t if point is at the beginning of the buffer.
  If the buffer is narrowed, this means the beginning of the narrowed part."
  (= 1 (point)))

(defun message-or-box (format-string &rest args)
  "Display a message in a dialog box or in the echo area.
  If this command was invoked with the mouse, use a dialog box if
  `use-dialog-box' is non-nil.
  Otherwise, use the echo area.
  The first argument is a format control string, and the rest are data
  to be formatted under control of the string.  See `format' for details.

  If the first argument is nil or the empty string, clear any existing
  message; let the minibuffer contents show."
  )

(defun propertize (string &rest properties)
  "Return a copy of STRING with text properties added.
  First argument is the string to copy.
  Remaining arguments form a sequence of PROPERTY VALUE pairs for text
  properties to add to the result."
  string)

(defun current-time-string (&optional specified-time)
  "Return the current local time, as a human-readable string.
  Programs can use this function to decode a time,
  since the number of columns in each field is fixed
  if the year is in the range 1000-9999.
  The format is `Sun Sep 16 01:03:52 1973'.
  However, see also the functions `decode-time' and `format-time-string'
  which provide a much more powerful and general facility.

  If SPECIFIED-TIME is given, it is a time to format instead of the
  current time.  The argument should have the form (HIGH LOW . IGNORED).
  Thus, you can use times obtained from `current-time' and from
  `file-attributes'.  SPECIFIED-TIME can also have the form (HIGH . LOW),
  but this is considered obsolete."
  (.format (SimpleDateFormat. "EEE MMM dd HH:mm:ss yyyy")
           (emacs-time-to-date (or specified-time (current-time)))))

(defun constrain-to-field (new-pos old-pos &optional escape-from-edge only-in-line inhibit-capture-property)
  "Return the position closest to NEW-POS that is in the same field as OLD-POS.
  A field is a region of text with the same `field' property.

  If NEW-POS is nil, then use the current point instead, and move point
  to the resulting constrained position, in addition to returning that
  position.

  If OLD-POS is at the boundary of two fields, then the allowable
  positions for NEW-POS depends on the value of the optional argument
  ESCAPE-FROM-EDGE: If ESCAPE-FROM-EDGE is nil, then NEW-POS is
  constrained to the field that has the same `field' char-property
  as any new characters inserted at OLD-POS, whereas if ESCAPE-FROM-EDGE
  is non-nil, NEW-POS is constrained to the union of the two adjacent
  fields.  Additionally, if two fields are separated by another field with
  the special value `boundary', then any point within this special field is
  also considered to be `on the boundary'.

  If the optional argument ONLY-IN-LINE is non-nil and constraining
  NEW-POS would move it to a different line, NEW-POS is returned
  unconstrained.  This useful for commands that move by line, like
  C-n or M-x beginning-of-line, which should generally respect field boundaries
  only in the case where they can still move to the right line.

  If the optional argument INHIBIT-CAPTURE-PROPERTY is non-nil, and OLD-POS has
  a non-nil property of that name, then any field boundaries are ignored.

  Field boundaries are not noticed if `inhibit-field-text-motion' is non-nil."
  )

(defun buffer-string ()
  "Return the contents of the current buffer as a string.
  If narrowing is in effect, this function returns only the visible part
  of the buffer."
  (str (.beg (.own-text (buffer/current-buffer)))))

(defun current-message ()
  "Return the string currently displayed in the echo area, or nil if none."
  ;; Not sure when and why it uses " *Echo Area 1*"
  (binding [buffer/*current-buffer* (buffer/get-buffer-create " *Echo Area 0*")]
    (buffer-string)))

(defun delete-field (&optional pos)
  "Delete the field surrounding POS.
  A field is a region of text with the same `field' property.
  If POS is nil, the value of point is used for POS."
  )

(defun delete-and-extract-region (start end)
  "Delete the text between START and END and return it."
  )

(defun current-time-zone (&optional specified-time)
  "Return the offset and name for the local time zone.
  This returns a list of the form (OFFSET NAME).
  OFFSET is an integer number of seconds ahead of UTC (east of Greenwich).
      A negative value means west of Greenwich.
  NAME is a string giving the name of the time zone.
  If SPECIFIED-TIME is given, the time zone offset is determined from it
  instead of using the current time.  The argument should have the form
  (HIGH LOW . IGNORED).  Thus, you can use times obtained from
  `current-time' and from `file-attributes'.  SPECIFIED-TIME can also
  have the form (HIGH . LOW), but this is considered obsolete.

  Some operating systems cannot provide all this information to Emacs;
  in this case, `current-time-zone' returns a list containing nil for
  the data it can't find."
  (let [specified-time (if specified-time
                         (emacs-time-to-date specified-time)
                         (Date.))
        timezone (.getTimeZone (doto (Calendar/getInstance)
                                 (.setTime specified-time)))]
    (list (/ (.getOffset timezone (.getTime specified-time)) 1000)
          (.getDisplayName timezone (.inDaylightTime timezone specified-time) TimeZone/SHORT))))

(defun insert-before-markers (&rest args)
  "Insert strings or characters at point, relocating markers after the text.
  Point and markers move forward to end up after the inserted text.

  If the current buffer is multibyte, unibyte strings are converted
  to multibyte for insertion (see `unibyte-char-to-multibyte').
  If the current buffer is unibyte, multibyte strings are converted
  to unibyte for insertion."
  (insert args))

(defun char-before (&optional pos)
  "Return character in current buffer preceding position POS.
  POS is an integer or a marker and defaults to point.
  If POS is out of range, the value is nil."
  (let [pos (- (or pos (point)) 2)]
    (when  (< -1 pos (buffer-size))
      (.charAt (buffer-string) pos))))

(defun char-to-string (char)
  "Convert arg CHAR to a string containing that character."
  (str (c/char char)))

(defun delete-region (start end)
  "Delete the text between START and END.
  If called interactively, delete the region between point and mark.
  This command deletes buffer text without modifying the kill ring."
  (let [text (.own-text (buffer/current-buffer))]
    (.delete (.beg text) (dec start) (dec end))
    (reset! (.modiff text) (System/currentTimeMillis))
    (when (> (point) start)
      (goto-char (+ (- end start) (- (point) start)))
      nil)))

(defun transpose-regions (startr1 endr1 startr2 endr2 &optional leave-markers)
  "Transpose region STARTR1 to ENDR1 with STARTR2 to ENDR2.
  The regions should not be overlapping, because the size of the buffer is
  never changed in a transposition.

  Optional fifth arg LEAVE-MARKERS, if non-nil, means don't update
  any markers that happen to be located in the regions.

  Transposing beyond buffer boundaries is an error."
  )

(defun goto-char (position)
  "Set point to POSITION, a number or marker.
  Beginning of buffer is position (point-min), end is (point-max).

  The return value is POSITION."
  (el/check-type 'integer-or-marker-p position)
  (let [position (if (data/markerp position) (.charpos position) position)
        real-pos (min (max 1 position) (inc (buffer-size)))]
    (reset! (.pt (buffer/current-buffer)) real-pos)
    (when-not (.mark-active (buffer/current-buffer))
      (reset! (.mark (buffer/current-buffer)) (point-marker)))
    position))

(defun insert-char (character count &optional inherit)
  "Insert COUNT copies of CHARACTER.
  Point, and before-insertion markers, are relocated as in the function `insert'.
  The optional third arg INHERIT, if non-nil, says to inherit text properties
  from adjoining text, if those properties are sticky."
  (insert (apply str (repeat count character))))

(defun system-name ()
  "Return the host name of the machine you are running on, as a string."
  globals/system-name)

(defun buffer-size (&optional buffer)
  "Return the number of characters in the current buffer.
  If BUFFER, return the number of characters in that buffer instead."
  (count (.beg (.own-text (or (and buffer (el/check-type 'bufferp buffer))
                          (buffer/current-buffer))))))

(defun region-end ()
  "Return the integer value of point or mark, whichever is larger."
  (max (.charpos (mark-marker)) (point)))

(defun format-time-string (format-string &optional time universal)
  "Use FORMAT-STRING to format the time TIME, or now if omitted.
  TIME is specified as (HIGH LOW . IGNORED), as returned by
  `current-time' or `file-attributes'.  The obsolete form (HIGH . LOW)
  is also still accepted.
  The third, optional, argument UNIVERSAL, if non-nil, means describe TIME
  as Universal Time; nil means describe TIME in the local time zone.
  The value is a copy of FORMAT-STRING, but with certain constructs replaced
  by text that describes the specified date and time in TIME:

  %Y is the year, %y within the century, %C the century.
  %G is the year corresponding to the ISO week, %g within the century.
  %m is the numeric month.
  %b and %h are the locale's abbreviated month name, %B the full name.
  %d is the day of the month, zero-padded, %e is blank-padded.
  %u is the numeric day of week from 1 (Monday) to 7, %w from 0 (Sunday) to 6.
  %a is the locale's abbreviated name of the day of week, %A the full name.
  %U is the week number starting on Sunday, %W starting on Monday,
   %V according to ISO 8601.
  %j is the day of the year.

  %H is the hour on a 24-hour clock, %I is on a 12-hour clock, %k is like %H
   only blank-padded, %l is like %I blank-padded.
  %p is the locale's equivalent of either AM or PM.
  %M is the minute.
  %S is the second.
  %N is the nanosecond, %6N the microsecond, %3N the millisecond, etc.
  %Z is the time zone name, %z is the numeric form.
  %s is the number of seconds since 1970-01-01 00:00:00 +0000.

  %c is the locale's date and time format.
  %x is the locale's \"preferred\" date format.
  %D is like \"%m/%d/%y\".

  %R is like \"%H:%M\", %T is like \"%H:%M:%S\", %r is like \"%I:%M:%S %p\".
  %X is the locale's \"preferred\" time format.

  Finally, %n is a newline, %t is a tab, %% is a literal %.

  Certain flags and modifiers are available with some format controls.
  The flags are `_', `-', `^' and `#'.  For certain characters X,
  %_X is like %X, but padded with blanks; %-X is like %X,
  but without padding.  %^X is like %X, but with all textual
  characters up-cased; %#X is like %X, but with letter-case of
  all textual characters reversed.
  %NX (where N stands for an integer) is like %X,
  but takes up at least N (a number) positions.
  The modifiers are `E' and `O'.  For certain characters X,
  %EX is a locale's alternative version of %X;
  %OX is like %X, but uses the locale's number symbols.

  For example, to produce full ISO 8601 format, use \"%Y-%m-%dT%T%z\"."
  (let [[hi low] (or time (current-time))
        time (* (+ (bit-shift-left hi 16) low) 1000)]
    (.format (SimpleDateFormat. (reduce #(apply s/replace %1 %2) format-string
                                        {"%Y" "Y"
                                         "%m" "MM"
                                         "%d" "dd"})) (Date. time))))

(defun insert-byte (byte count &optional inherit)
  "Insert COUNT (second arg) copies of BYTE (first arg).
  Both arguments are required.
  BYTE is a number of the range 0..255.

  If BYTE is 128..255 and the current buffer is multibyte, the
  corresponding eight-bit character is inserted.

  Point, and before-insertion markers, are relocated as in the function `insert'.
  The optional third arg INHERIT, if non-nil, says to inherit text properties
  from adjoining text, if those properties are sticky."
  (insert-char (char byte) count inherit))

(defun compare-buffer-substrings (buffer1 start1 end1 buffer2 start2 end2)
  "Compare two substrings of two buffers; return result as number.
  the value is -N if first string is less after N-1 chars,
  +N if first string is greater after N-1 chars, or 0 if strings match.
  Each substring is represented as three arguments: BUFFER, START and END.
  That makes six args in all, three for each substring.

  The value of `case-fold-search' in the current buffer
  determines whether case is significant or ignored."
  )

(defun mark-marker ()
  "Return this buffer's mark, as a marker object.
  Watch out!  Moving this marker changes the mark position.
  If you set the marker not to point anywhere, the buffer will have no mark."
  @(.mark (buffer/current-buffer)))

(defun user-full-name (&optional uid)
  "Return the full name of the user logged in, as a string.
  If the full name corresponding to Emacs's userid is not known,
  return \"unknown\".

  If optional argument UID is an integer or float, return the full name
  of the user with that uid, or nil if there is no such user.
  If UID is a string, return the full name of the user with that login
  name, or nil if there is no such user."
  )

(defun message (format-string &rest args)
  "Display a message at the bottom of the screen.
  The message also goes into the `*Messages*' buffer.
  (In keyboard macros, that's all it does.)
  Return the message.

  The first argument is a format control string, and the rest are data
  to be formatted under control of the string.  See `format' for details.

  Note: Use (message \"%s\" VALUE) to print the value of expressions and
  variables to avoid accidentally interpreting `%' as format specifiers.

  If the first argument is nil or the empty string, the function clears
  any existing message; this lets the minibuffer contents show.  See
  also `current-message'."
  (let [message (apply format format-string args)]
    (binding [buffer/*current-buffer* (buffer/get-buffer-create "*Messages*")]
      (insert (str message \newline))) ;; Emacs has logic to figure out if newline is needed.

    ;; The echo area buffers are called " *Echo Area %d*". They share window with the minibuffer.
    ;; Note the leading space for minibuffer window buffers.
    (binding [buffer/*current-buffer* (buffer/get-buffer-create " *Echo Area 0*")]
      (buffer/erase-buffer)
      (insert message))
    ;; This will go away
    (println message)))

(defun insert (&rest args)
  "Insert the arguments, either strings or characters, at point.
  Point and before-insertion markers move forward to end up
   after the inserted text.
  Any other markers at the point of insertion remain before the text.

  If the current buffer is multibyte, unibyte strings are converted
  to multibyte for insertion (see `string-make-multibyte').
  If the current buffer is unibyte, multibyte strings are converted
  to unibyte for insertion (see `string-make-unibyte').

  When operating on binary data, it may be necessary to preserve the
  original bytes of a unibyte string when inserting it into a multibyte
  buffer; to accomplish this, apply `string-as-multibyte' to the string
  and insert the result."
  (let [string (apply str args)
        pt @(.pt (buffer/current-buffer))
        text (.own-text (buffer/current-buffer))]
    (.insert (.beg text) (dec pt) string)
    (reset! (.modiff text) (System/currentTimeMillis))
    (goto-char (+ pt (count string))))
  nil)

(defun buffer-substring (start end)
  "Return the contents of part of the current buffer as a string.
  The two arguments START and END are character positions;
  they can be in either order.
  The string returned is multibyte if the buffer is multibyte.

  This function copies the text properties of that part of the buffer
  into the result string; if you don't want the text properties,
  use `buffer-substring-no-properties' instead."
  (subs (buffer-string) (dec start) (dec end)))

(defun byte-to-string (byte)
  "Convert arg BYTE to a unibyte string containing that byte."
  (str (char byte)))

(defun float-time (&optional specified-time)
  "Return the current time, as a float number of seconds since the epoch.
  If SPECIFIED-TIME is given, it is the time to convert to float
  instead of the current time.  The argument should have the form
  (HIGH LOW) or (HIGH LOW USEC). Thus, you can use times obtained from
  `current-time' and from `file-attributes'.  SPECIFIED-TIME can also
  have the form (HIGH . LOW), but this is considered obsolete.

  WARNING: Since the result is floating point, it may not be exact.
  If precise time stamps are required, use either `current-time',
  or (if you need time as a string) `format-time-string'."
  (let [specified-time (if specified-time
                         (emacs-time-to-date specified-time)
                         (Date.))]
    (/ (.getTime specified-time) 1000.0)))

(defun message-box (format-string &rest args)
  "Display a message, in a dialog box if possible.
  If a dialog box is not available, use the echo area.
  The first argument is a format control string, and the rest are data
  to be formatted under control of the string.  See `format' for details.

  If the first argument is nil or the empty string, clear any existing
  message; let the minibuffer contents show."
  )

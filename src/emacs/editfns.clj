(ns emacs.editfns (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun byte-to-position (bytepos)
  "Return the character position for byte position BYTEPOS."
  )

(defun field-string-no-properties (&optional pos)
  "Return the contents of the field around POS, without text properties.
  A field is a region of text with the same `field' property."
  )

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
  east of Greenwich.  (Note that Common Lisp has different meanings for"
  )

(defun current-time ()
  "Return the current time, as the number of seconds since 1970-01-01 00:00:00.
  The time is returned as a list of three integers.  The first has the
  most significant 16 bits of the seconds, while the second has the
  least significant 16 bits.  The third integer gives the microsecond
  count."
  )

(defun point-max-marker ()
  "Return a marker to the maximum permissible value of point in this buffer.
  This is (1+ (buffer-size)), unless narrowing (a buffer restriction)"
  )

(defun preceding-char ()
  "Return the character preceding point, as a number."
  )

(defun translate-region-internal (start end table)
  "Internal use only.
  From START to END, translate characters according to TABLE.
  TABLE is a string or a char-table; the Nth character in it is the
  mapping for the character with code N.
  It returns the number of characters changed.set-keyboard-coding-system-internal is a built-in function in `C
  source code'."
  )

(defun field-beginning (&optional pos escape-from-edge limit)
  "Return the beginning of the field surrounding POS.
  A field is a region of text with the same `field' property.
  If POS is nil, the value of point is used for POS.
  If ESCAPE-FROM-EDGE is non-nil and POS is at the beginning of its
  field, then the beginning of the *previous* field is returned.
  If LIMIT is non-nil, it is a buffer position; if the beginning of the field"
  )

(defun format (string &rest objects)
  "Format a string out of a format-string and arguments.
  The first argument is a format control string.
  The other arguments are substituted into it to make the result, a string."
  )

(defun user-uid ()
  "Return the effective uid of Emacs.
  Value is an integer or a float, depending on the value.register-code-conversion-map is a built-in function in `C source
  code'."
  )

(defun set-time-zone-rule (tz)
  "Set the local time zone using TZ, a string specifying a time zone rule.
  If TZ is nil, use implementation-defined default time zone information.
  If TZ is t, use Universal Time.next-single-char-property-change is a built-in function in `C source
  code'."
  )

(defun insert-and-inherit (&rest args)
  "Insert the arguments at point, inheriting properties from adjoining text.
  Point and before-insertion markers move forward to end up
   after the inserted text.
  Any other markers at the point of insertion remain before the text."
  )

(defun user-real-login-name ()
  "Return the name of the user's real uid, as a string.
  This ignores the environment variables LOGNAME and USER, so it differs from"
  )

(defun emacs-pid ()
  )

(defun point-max ()
  "Return the maximum permissible value of point in the current buffer.
  This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
  is in effect, in which case it is less.recursive-edit is an interactive built-in function in `C source code'."
  )

(defun char-equal (c1 c2)
  "Return t if two characters match, optionally ignoring case.
  Both arguments must be characters (i.e. integers)."
  )

(defun encode-time (second minute hour day month year &optional zone)
  "Convert SECOND, MINUTE, HOUR, DAY, MONTH, YEAR and ZONE to internal time.
  This is the reverse operation of `decode-time', which see.
  ZONE defaults to the current time zone rule.  This can
  be a string or t (as from `set-time-zone-rule'), or it can be a list
  (as from `current-time-zone') or an integer (as from `decode-time')
  applied without consideration for daylight saving time."
  )

(defun char-after (&optional pos)
  "Return character in current buffer at position POS.
  POS is an integer or a marker and defaults to point.
  If POS is out of range, the value is nil.kill-emacs is an interactive built-in function in `C source code'."
  )

(defun gap-size ()
  "Return the size of the current buffer's gap.
  See also `gap-position'.defmacro is a special form in `C source code'."
  )

(defun insert-buffer-substring (buffer &optional start end)
  "Insert before point a substring of the contents of BUFFER.
  BUFFER may be a buffer or a buffer name.
  Arguments START and END are character positions specifying the substring."
  )

(defun point-min-marker ()
  "Return a marker to the minimum permissible value of point in this buffer."
  )

(defun string-to-char (string)
  "Convert arg STRING to a character, the first character of that string.
  A multibyte character is handled correctly.unwind-protect is a special form in `C source code'."
  )

(defun point-marker ()
  )

(defun gap-position ()
  "Return the position of the gap, in the current buffer.
  See also `gap-size'.forward-word is an interactive built-in function in `C source code'."
  )

(defun eolp ()
  "Return t if point is at the end of a line."
  )

(defun line-end-position (&optional n)
  "Return the character position of the last character on the current line.
  With argument N not nil or 1, move forward N - 1 lines first.
  If scan reaches end of buffer, return that position."
  )

(defun get-internal-run-time ()
  "Return the current run time used by Emacs.
  The time is returned as a list of three integers.  The first has the
  most significant 16 bits of the seconds, while the second has the
  least significant 16 bits.  The third integer gives the microsecond
  count."
  )

(defun point-min ()
  "Return the minimum permissible value of point in the current buffer."
  )

(defun subst-char-in-region (start end fromchar tochar &optional noundo)
  "From START to END, replace FROMCHAR with TOCHAR each time it occurs.
  If optional arg NOUNDO is non-nil, don't record this change for undo
  and don't mark the buffer as really changed."
  )

(defun bolp ()
  )

(defun position-bytes (position)
  "Return the byte position for character position POSITION.
  If POSITION is out of range, the value is nil.set-terminal-coding-system-internal is a built-in function in `C
  source code'."
  )

(defun point ()
  "Return value of point, as an integer."
  )

(defun field-string (&optional pos)
  "Return the contents of the field surrounding POS as a string.
  A field is a region of text with the same `field' property."
  )

(defun region-beginning ()
  )

(defun line-beginning-position (&optional n)
  "Return the character position of the first character on the current line.
  With argument N not nil or 1, move forward N - 1 lines first.
  If scan reaches end of buffer, return that position."
  )

(defun following-char ()
  "Return the character following point, as a number."
  )

(defun eobp ()
  "Return t if point is at the end of the buffer."
  )

(defun user-real-uid ()
  "Return the real uid of Emacs.
  Value is an integer or a float, depending on the value.buffer-substring-no-properties is a built-in function in `C source
  code'."
  )

(defun field-end (&optional pos escape-from-edge limit)
  "Return the end of the field surrounding POS.
  A field is a region of text with the same `field' property.
  If POS is nil, the value of point is used for POS.
  If ESCAPE-FROM-EDGE is non-nil and POS is at the end of its field,
  then the end of the *following* field is returned.
  If LIMIT is non-nil, it is a buffer position; if the end of the field"
  )

(defun user-login-name (&optional uid)
  "Return the name under which the user logged in, as a string.
  This is based on the effective uid, not the real uid.
  Also, if the environment variables LOGNAME or USER are set,
  that determines the value of this function."
  )

(defun bobp ()
  "Return t if point is at the beginning of the buffer."
  )

(defun message-or-box (format-string &rest args)
  "Display a message in a dialog box or in the echo area.
  If this command was invoked with the mouse, use a dialog box if
  `use-dialog-box' is non-nil.
  Otherwise, use the echo area.
  The first argument is a format control string, and the rest are data
  to be formatted under control of the string.  See `format' for details."
  )

(defun propertize (string &rest properties)
  "Return a copy of STRING with text properties added.
  First argument is the string to copy.
  Remaining arguments form a sequence of PROPERTY VALUE pairs for text"
  )

(defun current-time-string (&optional specified-time)
  "Return the current local time, as a human-readable string.
  Programs can use this function to decode a time,
  since the number of columns in each field is fixed
  if the year is in the range 1000-9999.
  The format is `Sun Sep 16 01:03:52 1973'.
  However, see also the functions `decode-time' and `format-time-string'
  which provide a much more powerful and general facility."
  )

(defun constrain-to-field (new-pos old-pos &optional escape-from-edge only-in-line inhibit-capture-property)
  "Return the position closest to NEW-POS that is in the same field as OLD-POS."
  )

(defun buffer-string ()
  "Return the contents of the current buffer as a string.
  If narrowing is in effect, this function returns only the visible part"
  )

(defun current-message ()
  )

(defun delete-field (&optional pos)
  "Delete the field surrounding POS.
  A field is a region of text with the same `field' property.
  If POS is nil, the value of point is used for POS.previous-single-property-change is a built-in function in `C source
  code'."
  )

(defun delete-and-extract-region (start end)
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
  have the form (HIGH . LOW), but this is considered obsolete."
  )

(defun insert-before-markers (&rest args)
  "Insert strings or characters at point, relocating markers after the text.
  Point and markers move forward to end up after the inserted text."
  )

(defun char-before (&optional pos)
  "Return character in current buffer preceding position POS.
  POS is an integer or a marker and defaults to point."
  )

(defun char-to-string (char)
  )

(defun transpose-regions (startr1 endr1 startr2 endr2 &optional leave-markers)
  "Transpose region STARTR1 to ENDR1 with STARTR2 to ENDR2.
  The regions should not be overlapping, because the size of the buffer is
  never changed in a transposition."
  )

(defun insert-char (character count &optional inherit)
  "Insert COUNT copies of CHARACTER.
  Point, and before-insertion markers, are relocated as in the function `insert'.
  The optional third arg INHERIT, if non-nil, says to inherit text properties"
  )

(defun system-name ()
  )

(defun buffer-size (&optional buffer)
  "Return the number of characters in the current buffer."
  )

(defun region-end ()
  )

(defun format-time-string (format-string &optional time universal)
  "Use FORMAT-STRING to format the time TIME, or now if omitted.
  TIME is specified as (HIGH LOW . IGNORED), as returned by
  `current-time' or `file-attributes'.  The obsolete form (HIGH . LOW)
  is also still accepted.
  The third, optional, argument UNIVERSAL, if non-nil, means describe TIME
  as Universal Time; nil means describe TIME in the local time zone.
  The value is a copy of FORMAT-STRING, but with certain constructs replaced
  by text that describes the specified date and time in TIME:"
  )

(defun insert-byte (byte count &optional inherit)
  "Insert COUNT (second arg) copies of BYTE (first arg).
  Both arguments are required.
  BYTE is a number of the range 0..255."
  )

(defun compare-buffer-substrings (buffer1 start1 end1 buffer2 start2 end2)
  "Compare two substrings of two buffers; return result as number.
  the value is -N if first string is less after N-1 chars,
  +N if first string is greater after N-1 chars, or 0 if strings match.
  Each substring is represented as three arguments: BUFFER, START and END.
  That makes six args in all, three for each substring."
  )

(defun mark-marker ()
  "Return this buffer's mark, as a marker object.
  Watch out!  Moving this marker changes the mark position."
  )

(defun user-full-name (&optional uid)
  "Return the full name of the user logged in, as a string.
  If the full name corresponding to Emacs's userid is not known,
  return \"unknown\"."
  )

(defun message (format-string &rest args)
  "Display a message at the bottom of the screen.
  The message also goes into the `*Messages*' buffer.
  (In keyboard macros, that's all it does.)
  Return the message."
  )

(defun insert (&rest args)
  "Insert the arguments, either strings or characters, at point.
  Point and before-insertion markers move forward to end up
   after the inserted text.
  Any other markers at the point of insertion remain before the text."
  )

(defun buffer-substring (start end)
  "Return the contents of part of the current buffer as a string.
  The two arguments START and END are character positions;
  they can be in either order.
  The string returned is multibyte if the buffer is multibyte."
  )

(defun byte-to-string (byte)
  )

(defun float-time (&optional specified-time)
  "Return the current time, as a float number of seconds since the epoch.
  If SPECIFIED-TIME is given, it is the time to convert to float
  instead of the current time.  The argument should have the form
  (HIGH LOW) or (HIGH LOW USEC). Thus, you can use times obtained from
  `current-time' and from `file-attributes'.  SPECIFIED-TIME can also
  have the form (HIGH . LOW), but this is considered obsolete."
  )

(defun message-box (format-string &rest args)
  "Display a message, in a dialog box if possible.
  If a dialog box is not available, use the echo area.
  The first argument is a format control string, and the rest are data
  to be formatted under control of the string.  See `format' for details."
  )

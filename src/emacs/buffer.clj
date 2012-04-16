(ns emacs.buffer (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun barf-if-buffer-read-only ()
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  )

(defun overlayp (object)
  "Return t if OBJECT is an overlay.end-kbd-macro is an interactive built-in function in `C source code'."
  )

(defun make-overlay (beg end &optional buffer front-advance rear-advance)
  "Create a new overlay with range BEG to END in BUFFER.
  If omitted, BUFFER defaults to the current buffer.
  BEG and END may be integers or markers.
  The fourth arg FRONT-ADVANCE, if non-nil, makes the marker
  for the front of the overlay advance when text is inserted there
  (which means the text *is not* included in the overlay).
  The fifth arg REAR-ADVANCE, if non-nil, makes the marker
  for the rear of the overlay advance when text is inserted there
  (which means the text *is* included in the overlay)."
  )

(defun buffer-live-p (object)
  "Return non-nil if OBJECT is a buffer which has not been killed.
  Value is nil if OBJECT is not a buffer or if it has been killed."
  )

(defun restore-buffer-modified-p (flag)
  "Like `set-buffer-modified-p', with a difference concerning redisplay.
  It is not ensured that mode lines will be updated to show the modified
  state of the current buffer.  Use with care."
  )

(defun buffer-modified-p (&optional buffer)
  "Return t if BUFFER was modified since its file was last read or saved.
  No argument or nil as argument means use current buffer as BUFFER."
  )

(defun buffer-chars-modified-tick (&optional buffer)
  "Return BUFFER's character-change tick counter.
  Each buffer has a character-change tick counter, which is set to the
  value of the buffer's tick counter (see `buffer-modified-tick'), each
  time text in that buffer is inserted or deleted.  By comparing the
  values returned by two individual calls of `buffer-chars-modified-tick',
  you can tell whether a character change occurred in that buffer in
  between these calls.  No argument or nil as argument means use current
  buffer as BUFFER."
  )

(defun generate-new-buffer-name (name &optional ignore)
  "Return a string that is the name of no existing buffer based on NAME.
  If there is no live buffer named NAME, then return NAME.
  Otherwise modify name by appending `<NUMBER>', incrementing NUMBER
  (starting at 2) until an unused name is found, and then return that name.
  Optional second argument IGNORE specifies a name that is okay to use (if
  it is in the sequence to be tried) even if a buffer with that name exists."
  )

(defun set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG.
  If FLAG is t, this makes the buffer a multibyte buffer.
  If FLAG is nil, this makes the buffer a single-byte buffer.
  In these cases, the buffer contents remain unchanged as a sequence of
  bytes but the contents viewed as characters do change.
  If FLAG is `to', this makes the buffer a multibyte buffer by changing
  all eight-bit bytes to eight-bit characters.
  If the multibyte flag was really changed, undo information of the
  current buffer is cleared."
  )

(defun overlay-recenter (pos)
  "Recenter the overlays of the current buffer around position POS.
  That makes overlay lookup faster for positions near POS (but perhaps slower
  for positions far away from POS)."
  )

(defun get-buffer-create (buffer-or-name)
  "Return the buffer specified by BUFFER-OR-NAME, creating a new one if needed.
  If BUFFER-OR-NAME is a string and a live buffer with that name exists,
  return that buffer.  If no such buffer exists, create a new buffer with
  that name and return it.  If BUFFER-OR-NAME starts with a space, the new
  buffer does not keep undo information."
  )

(defun overlay-start (overlay)
  "Return the position at which OVERLAY starts."
  )

(defun get-buffer (buffer-or-name)
  "Return the buffer named BUFFER-OR-NAME.
  BUFFER-OR-NAME must be either a string or a buffer.  If BUFFER-OR-NAME
  is a string and there is no buffer with that name, return nil.  If
  BUFFER-OR-NAME is a buffer, return it as given."
  )

(defun current-buffer ()
  "Return the current buffer as a Lisp object."
  )

(defun delete-overlay (overlay)
  "Delete the overlay OVERLAY from its buffer."
  )

(defun buffer-base-buffer (&optional buffer)
  "Return the base buffer of indirect buffer BUFFER.
  If BUFFER is not indirect, return nil.
  BUFFER defaults to the current buffer."
  )

(defun overlay-buffer (overlay)
  "Return the buffer OVERLAY belongs to.
  Return nil if OVERLAY has been deleted."
  )

(defun kill-all-local-variables ()
  "Switch to Fundamental mode by killing current buffer's local variables.
  Most local variable bindings are eliminated so that the default values
  become effective once more.  Also, the syntax table is set from
  `standard-syntax-table', the local keymap is set to nil,
  and the abbrev table from `fundamental-mode-abbrev-table'.
  This function also forces redisplay of the mode line."
  )

(defun overlay-end (overlay)
  "Return the position at which OVERLAY ends."
  )

(defun buffer-name (&optional buffer)
  "Return the name of BUFFER, as a string.
  BUFFER defaults to the current buffer.
  Return nil if BUFFER has been killed."
  )

(defun overlay-put (overlay prop value)
  "Set one property of overlay OVERLAY: give property PROP value VALUE.encode-coding-region is an interactive built-in function in `C source
  code'."
  )

(defun set-buffer (buffer-or-name)
  "Make buffer BUFFER-OR-NAME current for editing operations.
  BUFFER-OR-NAME may be a buffer or the name of an existing buffer.  See
  also `save-excursion' when you want to make a buffer current
  temporarily.  This function does not display the buffer, so its effect
  ends when the current command terminates.  Use `switch-to-buffer' or
  `pop-to-buffer' to switch buffers permanently."
  )

(defun buffer-list (&optional frame)
  "Return a list of all existing live buffers.
  If the optional arg FRAME is a frame, we return the buffer list
  in the proper order for that frame: the buffers in FRAME's `buffer-list'
  frame parameter come first, followed by the rest of the buffers."
  )

(defun previous-overlay-change (pos)
  "Return the previous position before POS where an overlay starts or ends.
  If there are no overlay boundaries from (point-min) to POS,
  the value is (point-min)."
  )

(defun buffer-local-variables (&optional buffer)
  "Return an alist of variables that are buffer-local in BUFFER.
  Most elements look like (SYMBOL . VALUE), describing one variable.
  For a symbol that is locally unbound, just the symbol appears in the value.
  Note that storing new VALUEs in these elements doesn't change the variables.
  No argument or nil as argument means use current buffer as BUFFER.delete-frame is an interactive built-in function in `C source code'."
  )

(defun overlays-in (beg end)
  "Return a list of the overlays that overlap the region BEG ... END.
  Overlap means that at least one character is contained within the overlay
  and also contained within the specified region.
  Empty overlays are included in the result if they are located at BEG,
  between BEG and END, or at END provided END denotes the position at the
  end of the buffer."
  )

(defun next-overlay-change (pos)
  "Return the next position after POS where an overlay starts or ends.
  If there are no overlay boundaries from POS to (point-max),
  the value is (point-max)."
  )

(defun buffer-swap-text (buffer)
  "Swap the text between current buffer and BUFFER.unix-sync is an interactive built-in function in `C source code'."
  )

(defun overlay-get (overlay prop)
  "Get the property of overlay OVERLAY with property name PROP."
  )

(defun overlay-lists ()
  "Return a pair of lists giving all the overlays of the current buffer.
  The car has all the overlays before the overlay center;
  the cdr has all the overlays after the overlay center.
  Recentering overlays moves overlays between these lists.
  The lists you get are copies, so that changing them has no effect.
  However, the overlays you get are the real objects that the buffer uses."
  )

(defun set-buffer-modified-p (flag)
  "Mark current buffer as modified or unmodified according to FLAG.
  A non-nil FLAG means mark the buffer modified.rename-file is an interactive built-in function in `C source code'."
  )

(defun move-overlay (overlay beg end &optional buffer)
  "Set the endpoints of OVERLAY to BEG and END in BUFFER.
  If BUFFER is omitted, leave OVERLAY in the same buffer it inhabits now.
  If BUFFER is omitted, and OVERLAY is in no buffer, put it in the current
  buffer."
  )

(defun buffer-modified-tick (&optional buffer)
  "Return BUFFER's tick counter, incremented for each change in text.
  Each buffer has a tick counter which is incremented each time the
  text in that buffer is changed.  It wraps around occasionally.
  No argument or nil as argument means use current buffer as BUFFER."
  )

(defun buffer-file-name (&optional buffer)
  "Return name of file BUFFER is visiting, or nil if none.
  No argument or nil as argument means use the current buffer."
  )

(defun buffer-local-value (variable buffer)
  "Return the value of VARIABLE in BUFFER.
  If VARIABLE does not have a buffer-local binding in BUFFER, the value
  is the default binding of the variable."
  )

(defun get-file-buffer (filename)
  "Return the buffer visiting file FILENAME (a string).
  The buffer's `buffer-file-name' must match exactly the expansion of FILENAME.
  If there is no such live buffer, return nil.
  See also `find-buffer-visiting'."
  )

(defun overlay-properties (overlay)
  "Return a list of the properties on OVERLAY.
  This is a copy of OVERLAY's plist; modifying its conses has no effect on
  OVERLAY."
  )

(defun other-buffer (&optional buffer visible-ok frame)
  "Return most recently selected buffer other than BUFFER.
  Buffers not visible in windows are preferred to visible buffers,
  unless optional second argument VISIBLE-OK is non-nil.
  If the optional third argument FRAME is non-nil, use that frame's
  buffer list instead of the selected frame's buffer list.
  If no other buffer exists, the buffer `*scratch*' is returned.
  If BUFFER is omitted or nil, some interesting buffer is returned."
  )

(defun overlays-at (pos)
  "Return a list of the overlays that contain the character at POS."
  )

(defun set-buffer-major-mode (buffer)
  "Set an appropriate major mode for BUFFER.
  For the *scratch* buffer, use `initial-major-mode', otherwise choose a mode
  according to `default-major-mode'.
  Use this function before selecting the buffer, since it may need to inspect
  the current buffer's major mode."
  )

(ns emacs.buffer (use [deuce.core]) (:refer-clojure :only []))

(defun barf-if-buffer-read-only ()
  )

(defun overlayp (object)
  "Return t if OBJECT is an overlay.end-kbd-macro is an interactive built-in function in `C source code'."
  )

(defun make-overlay (beg end &optional buffer front-advance rear-advance)
  "Create a new overlay with range BEG to END in BUFFER.\nIf omitted, BUFFER defaults to the current buffer.\nBEG and END may be integers or markers.\nThe fourth arg FRONT-ADVANCE, if non-nil, makes the marker\nfor the front of the overlay advance when text is inserted there\n(which means the text *is not* included in the overlay).\nThe fifth arg REAR-ADVANCE, if non-nil, makes the marker\nfor the rear of the overlay advance when text is inserted there\n"
  )

(defun buffer-live-p (object)
  "Return non-nil if OBJECT is a buffer which has not been killed.\n"
  )

(defun restore-buffer-modified-p (flag)
  "Like `set-buffer-modified-p', with a difference concerning redisplay.\nIt is not ensured that mode lines will be updated to show the modified\n"
  )

(defun buffer-modified-p (&optional buffer)
  "Return t if BUFFER was modified since its file was last read or saved.\n"
  )

(defun buffer-chars-modified-tick (&optional buffer)
  "Return BUFFER's character-change tick counter.\nEach buffer has a character-change tick counter, which is set to the\nvalue of the buffer's tick counter (see `buffer-modified-tick'), each\ntime text in that buffer is inserted or deleted.  By comparing the\nvalues returned by two individual calls of `buffer-chars-modified-tick',\nyou can tell whether a character change occurred in that buffer in\nbetween these calls.  No argument or nil as argument means use current\n"
  )

(defun generate-new-buffer-name (name &optional ignore)
  "Return a string that is the name of no existing buffer based on NAME.\nIf there is no live buffer named NAME, then return NAME.\nOtherwise modify name by appending `<NUMBER>', incrementing NUMBER\n(starting at 2) until an unused name is found, and then return that name.\nOptional second argument IGNORE specifies a name that is okay to use (if\n"
  )

(defun set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG.\nIf FLAG is t, this makes the buffer a multibyte buffer.\nIf FLAG is nil, this makes the buffer a single-byte buffer.\nIn these cases, the buffer contents remain unchanged as a sequence of\nbytes but the contents viewed as characters do change.\nIf FLAG is `to', this makes the buffer a multibyte buffer by changing\nall eight-bit bytes to eight-bit characters.\nIf the multibyte flag was really changed, undo information of the\n"
  )

(defun overlay-recenter (pos)
  "Recenter the overlays of the current buffer around position POS.\nThat makes overlay lookup faster for positions near POS (but perhaps slower\n"
  )

(defun get-buffer-create (buffer-or-name)
  "Return the buffer specified by BUFFER-OR-NAME, creating a new one if needed.\nIf BUFFER-OR-NAME is a string and a live buffer with that name exists,\nreturn that buffer.  If no such buffer exists, create a new buffer with\nthat name and return it.  If BUFFER-OR-NAME starts with a space, the new\nbuffer does not keep undo information."
  )

(defun overlay-start (overlay)
  "Return the position at which OVERLAY starts.let is a special form in `C source code'."
  )

(defun get-buffer (buffer-or-name)
  "Return the buffer named BUFFER-OR-NAME.\nBUFFER-OR-NAME must be either a string or a buffer.  If BUFFER-OR-NAME\nis a string and there is no buffer with that name, return nil.  If\n"
  )

(defun current-buffer ()
  )

(defun delete-overlay (overlay)
  )

(defun buffer-base-buffer (&optional buffer)
  "Return the base buffer of indirect buffer BUFFER.\nIf BUFFER is not indirect, return nil.\n"
  )

(defun overlay-buffer (overlay)
  "Return the buffer OVERLAY belongs to.\n"
  )

(defun kill-all-local-variables ()
  "Switch to Fundamental mode by killing current buffer's local variables.\nMost local variable bindings are eliminated so that the default values\nbecome effective once more.  Also, the syntax table is set from\n`standard-syntax-table', the local keymap is set to nil,\nand the abbrev table from `fundamental-mode-abbrev-table'.\nThis function also forces redisplay of the mode line."
  )

(defun overlay-end (overlay)
  )

(defun buffer-name (&optional buffer)
  "Return the name of BUFFER, as a string.\nBUFFER defaults to the current buffer.\n"
  )

(defun overlay-put (overlay prop value)
  "Set one property of overlay OVERLAY: give property PROP value VALUE.encode-coding-region is an interactive built-in function in `C source\ncode'."
  )

(defun set-buffer (buffer-or-name)
  "Make buffer BUFFER-OR-NAME current for editing operations.\nBUFFER-OR-NAME may be a buffer or the name of an existing buffer.  See\nalso `save-excursion' when you want to make a buffer current\ntemporarily.  This function does not display the buffer, so its effect\nends when the current command terminates.  Use `switch-to-buffer' or\n`pop-to-buffer' to switch buffers permanently.setq-default is a special form in `C source code'."
  )

(defun buffer-list (&optional frame)
  "Return a list of all existing live buffers.\nIf the optional arg FRAME is a frame, we return the buffer list\nin the proper order for that frame: the buffers in FRAME's `buffer-list'\n"
  )

(defun previous-overlay-change (pos)
  "Return the previous position before POS where an overlay starts or ends.\nIf there are no overlay boundaries from (point-min) to POS,\n"
  )

(defun buffer-local-variables (&optional buffer)
  "Return an alist of variables that are buffer-local in BUFFER.\nMost elements look like (SYMBOL . VALUE), describing one variable.\nFor a symbol that is locally unbound, just the symbol appears in the value.\nNote that storing new VALUEs in these elements doesn't change the variables.\nNo argument or nil as argument means use current buffer as BUFFER.delete-frame is an interactive built-in function in `C source code'."
  )

(defun overlays-in (beg end)
  "Return a list of the overlays that overlap the region BEG ... END.\nOverlap means that at least one character is contained within the overlay\nand also contained within the specified region.\nEmpty overlays are included in the result if they are located at BEG,\nbetween BEG and END, or at END provided END denotes the position at the\n"
  )

(defun next-overlay-change (pos)
  "Return the next position after POS where an overlay starts or ends.\nIf there are no overlay boundaries from POS to (point-max),\n"
  )

(defun buffer-swap-text (buffer)
  "Swap the text between current buffer and BUFFER.unix-sync is an interactive built-in function in `C source code'."
  )

(defun overlay-get (overlay prop)
  )

(defun overlay-lists ()
  "Return a pair of lists giving all the overlays of the current buffer.\nThe car has all the overlays before the overlay center;\nthe cdr has all the overlays after the overlay center.\nRecentering overlays moves overlays between these lists.\nThe lists you get are copies, so that changing them has no effect.\n"
  )

(defun set-buffer-modified-p (flag)
  "Mark current buffer as modified or unmodified according to FLAG.\nA non-nil FLAG means mark the buffer modified.rename-file is an interactive built-in function in `C source code'."
  )

(defun move-overlay (overlay beg end &optional buffer)
  "Set the endpoints of OVERLAY to BEG and END in BUFFER.\nIf BUFFER is omitted, leave OVERLAY in the same buffer it inhabits now.\nIf BUFFER is omitted, and OVERLAY is in no buffer, put it in the current\n"
  )

(defun buffer-modified-tick (&optional buffer)
  "Return BUFFER's tick counter, incremented for each change in text.\nEach buffer has a tick counter which is incremented each time the\ntext in that buffer is changed.  It wraps around occasionally.\n"
  )

(defun buffer-file-name (&optional buffer)
  "Return name of file BUFFER is visiting, or nil if none.\n"
  )

(defun buffer-local-value (variable buffer)
  "Return the value of VARIABLE in BUFFER.\nIf VARIABLE does not have a buffer-local binding in BUFFER, the value\n"
  )

(defun get-file-buffer (filename)
  "Return the buffer visiting file FILENAME (a string).\nThe buffer's `buffer-file-name' must match exactly the expansion of FILENAME.\nIf there is no such live buffer, return nil.\n"
  )

(defun overlay-properties (overlay)
  "Return a list of the properties on OVERLAY.\nThis is a copy of OVERLAY's plist; modifying its conses has no effect on\n"
  )

(defun other-buffer (&optional buffer visible-ok frame)
  "Return most recently selected buffer other than BUFFER.\nBuffers not visible in windows are preferred to visible buffers,\nunless optional second argument VISIBLE-OK is non-nil.\nIf the optional third argument FRAME is non-nil, use that frame's\nbuffer list instead of the selected frame's buffer list.\nIf no other buffer exists, the buffer `*scratch*' is returned.\n"
  )

(defun overlays-at (pos)
  )

(defun set-buffer-major-mode (buffer)
  "Set an appropriate major mode for BUFFER.\nFor the *scratch* buffer, use `initial-major-mode', otherwise choose a mode\naccording to `default-major-mode'.\nUse this function before selecting the buffer, since it may need to inspect\n"
  )

(ns deuce.emacs.buffer
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.data :as data]
            [deuce.emacs.eval :as eval]
            [deuce.emacs-lisp :as el]
            [deuce.emacs-lisp.cons :as cons]
            [deuce.emacs-lisp.globals :as globals])
  (:import [deuce.emacs.data Buffer BufferText Marker]
           [clojure.lang Var]
           [java.io PushbackReader])
  (:refer-clojure :exclude []))

(defvar before-change-functions nil
  "List of functions to call before each text change.
  Two arguments are passed to each function: the positions of
  the beginning and end of the range of old text to be changed.
  (For an insertion, the beginning and end are at the same place.)
  No information is given about the length of the text after the change.

  Buffer changes made while executing the `before-change-functions'
  don't call any before-change or after-change functions.
  That's because `inhibit-modification-hooks' is temporarily set non-nil.

  If an unhandled error happens in running these functions,
  the variable's value remains nil.  That prevents the error
  from happening repeatedly and making Emacs nonfunctional.")

(defvar default-ctl-arrow nil
  "Default value of `ctl-arrow' for buffers that do not override it.
  This is the same as (default-value 'ctl-arrow).")

(defvar kill-buffer-hook nil
  "Hook to be run (by `run-hooks', which see) when a buffer is killed.
  The buffer being killed will be current while the hook is running.
  See `kill-buffer'.")

(defvar default-line-spacing nil
  "Default value of `line-spacing' for buffers that don't override it.
  This is the same as (default-value 'line-spacing).")

(defvar transient-mark-mode nil
  "Non-nil if Transient Mark mode is enabled.
  See the command `transient-mark-mode' for a description of this minor mode.

  Non-nil also enables highlighting of the region whenever the mark is active.
  The variable `highlight-nonselected-windows' controls whether to highlight
  all windows or just the selected window.

  Lisp programs may give this variable certain special values:

  - A value of `lambda' enables Transient Mark mode temporarily.
    It is disabled again after any subsequent action that would
    normally deactivate the mark (e.g. buffer modification).

  - A value of (only . OLDVAL) enables Transient Mark mode
    temporarily.  After any subsequent point motion command that is
    not shift-translated, or any other action that would normally
    deactivate the mark (e.g. buffer modification), the value of
    `transient-mark-mode' is set to OLDVAL.

  You can customize this variable.")

(defvar buffer-display-count nil
  "A number incremented each time this buffer is displayed in a window.
  The function `set-window-buffer' increments it.")

(defvar vertical-scroll-bar true
  "*Position of this buffer's vertical scroll bar.
  The value takes effect whenever you tell a window to display this buffer;
  for instance, with `set-window-buffer' or when `display-buffer' displays it.

  A value of `left' or `right' means put the vertical scroll bar at that side
  of the window; a value of nil means don't show any vertical scroll bars.
  A value of t (the default) means do whatever the window's frame specifies.")

(defvar default-indicate-empty-lines nil
  "Default value of `indicate-empty-lines' for buffers that don't override it.
  This is the same as (default-value 'indicate-empty-lines).")

(defvar cache-long-line-scans nil
  "Non-nil means that Emacs should use caches to handle long lines more quickly.

  Normally, the line-motion functions work by scanning the buffer for
  newlines.  Columnar operations (like `move-to-column' and
  `compute-motion') also work by scanning the buffer, summing character
  widths as they go.  This works well for ordinary text, but if the
  buffer's lines are very long (say, more than 500 characters), these
  motion functions will take longer to execute.  Emacs may also take
  longer to update the display.

  If `cache-long-line-scans' is non-nil, these motion functions cache the
  results of their scans, and consult the cache to avoid rescanning
  regions of the buffer until the text is modified.  The caches are most
  beneficial when they prevent the most searching---that is, when the
  buffer contains long lines and large regions of characters with the
  same, fixed screen width.

  When `cache-long-line-scans' is non-nil, processing short lines will
  become slightly slower (because of the overhead of consulting the
  cache), and the caches will use memory roughly proportional to the
  number of newlines and characters whose screen width varies.

  The caches require no explicit maintenance; their accuracy is
  maintained internally by the Emacs primitives.  Enabling or disabling
  the cache should not affect the behavior of any of the motion
  functions; it should only affect their performance.")

(defvar default-enable-multibyte-characters nil
  "*Default value of `enable-multibyte-characters' for buffers not overriding it.
  This is the same as (default-value 'enable-multibyte-characters).")

(defvar default-fringes-outside-margins nil
  "Default value of `fringes-outside-margins' for buffers that don't override it.
  This is the same as (default-value 'fringes-outside-margins).")

(defvar mode-name "Fundamental"
  "Pretty name of current buffer's major mode.
  Usually a string, but can use any of the constructs for `mode-line-format',
  which see.
  Format with `format-mode-line' to produce a string value.")

(defvar buffer-undo-list nil
  "List of undo entries in current buffer.
  Recent changes come first; older changes follow newer.

  An entry (BEG . END) represents an insertion which begins at
  position BEG and ends at position END.

  An entry (TEXT . POSITION) represents the deletion of the string TEXT
  from (abs POSITION).  If POSITION is positive, point was at the front
  of the text being deleted; if negative, point was at the end.

  An entry (t HIGH . LOW) indicates that the buffer previously had
  \"unmodified\" status.  HIGH and LOW are the high and low 16-bit portions
  of the visited file's modification time, as of that time.  If the
  modification time of the most recent save is different, this entry is
  obsolete.

  An entry (nil PROPERTY VALUE BEG . END) indicates that a text property
  was modified between BEG and END.  PROPERTY is the property name,
  and VALUE is the old value.

  An entry (apply FUN-NAME . ARGS) means undo the change with
  (apply FUN-NAME ARGS).

  An entry (apply DELTA BEG END FUN-NAME . ARGS) supports selective undo
  in the active region.  BEG and END is the range affected by this entry
  and DELTA is the number of bytes added or deleted in that range by
  this change.

  An entry (MARKER . DISTANCE) indicates that the marker MARKER
  was adjusted in position by the offset DISTANCE (an integer).

  An entry of the form POSITION indicates that point was at the buffer
  location given by the integer.  Undoing an entry of this form places
  point at POSITION.

  Entries with value `nil' mark undo boundaries.  The undo command treats
  the changes between two undo boundaries as a single step to be undone.

  If the value of the variable is t, undo information is not recorded.")

(defvar buffer-file-truename nil
  "Abbreviated truename of file visited in current buffer, or nil if none.
  The truename of a file is calculated by `file-truename'
  and then abbreviated with `abbreviate-file-name'.")

(defvar right-fringe-width nil
  "*Width of this buffer's right fringe (in pixels).
  A value of 0 means no right fringe is shown in this buffer's window.
  A value of nil means to use the right fringe width from the window's frame.")

(defvar default-scroll-bar-width nil
  "Default value of `scroll-bar-width' for buffers that don't override it.
  This is the same as (default-value 'scroll-bar-width).")

(defvar kill-buffer-query-functions nil
  "List of functions called with no args to query before killing a buffer.
  The buffer being killed will be current while the functions are running.
  If any of them returns nil, the buffer is not killed.")

(defvar overwrite-mode nil
  "Non-nil if self-insertion should replace existing text.
  The value should be one of `overwrite-mode-textual',
  `overwrite-mode-binary', or nil.
  If it is `overwrite-mode-textual', self-insertion still
  inserts at the end of a line, and inserts when point is before a tab,
  until the tab is filled in.
  If `overwrite-mode-binary', self-insertion replaces newlines and tabs too.")

(defvar default-right-fringe-width nil
  "Default value of `right-fringe-width' for buffers that don't override it.
  This is the same as (default-value 'right-fringe-width).")

(defvar word-wrap nil
  "*Non-nil means to use word-wrapping for continuation lines.
  When word-wrapping is on, continuation lines are wrapped at the space
  or tab character nearest to the right window edge.
  If nil, continuation lines are wrapped at the right screen edge.

  This variable has no effect if long lines are truncated (see
  `truncate-lines' and `truncate-partial-width-windows').  If you use
  word-wrapping, you might want to reduce the value of
  `truncate-partial-width-windows', since wrapping can make text readable
  in narrower windows.

  You can customize this variable.")

(defvar buffer-auto-save-file-format true
  "*Format in which to write auto-save files.
  Should be a list of symbols naming formats that are defined in `format-alist'.
  If it is t, which is the default, auto-save files are written in the
  same format as a regular save would use.")

(defvar default-major-mode nil
  "*Value of `major-mode' for new buffers.")

(defvar default-left-margin nil
  "Default value of `left-margin' for buffers that do not override it.
  This is the same as (default-value 'left-margin).")

(defvar mode-line-format "%-"
  "Template for displaying mode line for current buffer.
  Each buffer has its own value of this variable.
  Value may be nil, a string, a symbol or a list or cons cell.
  A value of nil means don't display a mode line.
  For a symbol, its value is used (but it is ignored if t or nil).
   A string appearing directly as the value of a symbol is processed verbatim
   in that the %-constructs below are not recognized.
   Note that unless the symbol is marked as a `risky-local-variable', all
   properties in any strings, as well as all :eval and :propertize forms
   in the value of that symbol will be ignored.
  For a list of the form `(:eval FORM)', FORM is evaluated and the result
   is used as a mode line element.  Be careful--FORM should not load any files,
   because that can cause an infinite recursion.
  For a list of the form `(:propertize ELT PROPS...)', ELT is displayed
   with the specified properties PROPS applied.
  For a list whose car is a symbol, the symbol's value is taken,
   and if that is non-nil, the cadr of the list is processed recursively.
   Otherwise, the caddr of the list (if there is one) is processed.
  For a list whose car is a string or list, each element is processed
   recursively and the results are effectively concatenated.
  For a list whose car is an integer, the cdr of the list is processed
    and padded (if the number is positive) or truncated (if negative)
    to the width specified by that number.
  A string is printed verbatim in the mode line except for %-constructs:
    (%-constructs are allowed when the string is the entire mode-line-format
     or when it is found in a cons-cell or a list)
    %b -- print buffer name.      %f -- print visited file name.
    %F -- print frame name.
    %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.
  	%& is like %*, but ignore read-only-ness.
  	% means buffer is read-only and * means it is modified.
  	For a modified read-only buffer, %* gives % and %+ gives *.
    %s -- print process status.   %l -- print the current line number.
    %c -- print the current column number (this makes editing slower).
          To make the column number update correctly in all cases,
  	`column-number-mode' must be non-nil.
    %i -- print the size of the buffer.
    %I -- like %i, but use k, M, G, etc., to abbreviate.
    %p -- print percent of buffer above top of window, or Top, Bot or All.
    %P -- print percent of buffer above bottom of window, perhaps plus Top,
          or print Bottom or All.
    %n -- print Narrow if appropriate.
    %t -- visited file is text or binary (if OS supports this distinction).
    %z -- print mnemonics of keyboard, terminal, and buffer coding systems.
    %Z -- like %z, but including the end-of-line format.
    %e -- print error message about full memory.
    %@ -- print @ or hyphen.  @ means that default-directory is on a
          remote machine.
    %[ -- print one [ for each recursive editing level.  %] similar.
    %% -- print %.   %- -- print infinitely many dashes.
  Decimal digits after the % specify field width to which to pad.

  You can customize this variable.")

(defvar change-major-mode-hook nil
  "Normal hook run before changing the major mode of a buffer.
  The function `kill-all-local-variables' runs this before doing anything else.")

(defvar scroll-down-aggressively nil
  "How far to scroll windows downward.
  If you move point off the top, the window scrolls automatically.
  This variable controls how far it scrolls.  The value nil, the default,
  means scroll to center point.  A fraction means scroll to put point
  that fraction of the window's height from the top of the window.
  When the value is 0.0, point goes at the top line, which in the
  simple case that you moved off with C-b means scrolling just one line.
  1.0 means point goes at the bottom, so that in that simple case, the
  window scrolls by a full window height.  Meaningful values are
  between 0.0 and 1.0, inclusive.

  You can customize this variable.")

(defvar fringe-cursor-alist nil
  "*Mapping from logical to physical fringe cursor bitmaps.
  The value is an alist where each element (CURSOR . BITMAP)
  specifies the fringe bitmaps used to display a specific logical
  cursor type in the fringe.

  CURSOR specifies the logical cursor type which is one of the following
  symbols: `box' , `hollow', `bar', `hbar', or `hollow-small'.  The last
  one is used to show a hollow cursor on narrow lines display lines
  where the normal hollow cursor will not fit.

  BITMAP is the corresponding fringe bitmap shown for the logical
  cursor type.")

(defvar auto-fill-function nil
  "Function called (if non-nil) to perform auto-fill.
  It is called after self-inserting any character specified in
  the `auto-fill-chars' table.
  NOTE: This variable is not a hook;
  its value may not be a list of functions.")

(defvar point-before-scroll nil
  "Value of point before the last series of scroll operations, or nil.")

(defvar right-margin-width 0
  "*Width of right marginal area for display of a buffer.
  A value of nil means no marginal area.")

(defvar default-cursor-type nil
  "Default value of `cursor-type' for buffers that don't override it.
  This is the same as (default-value 'cursor-type).")

(defvar scroll-bar-width nil
  "*Width of this buffer's scroll bars in pixels.
  A value of nil means to use the scroll bar width from the window's frame.")

(defvar buffer-display-table nil
  "Display table that controls display of the contents of current buffer.

  If this variable is nil, the value of `standard-display-table' is used.
  Each window can have its own, overriding display table, see
  `set-window-display-table' and `window-display-table'.

  The display table is a char-table created with `make-display-table'.
  A char-table is an array indexed by character codes.  Normal array
  primitives `aref' and `aset' can be used to access elements of a char-table.

  Each of the char-table elements control how to display the corresponding
  text character: the element at index C in the table says how to display
  the character whose code is C.  Each element should be a vector of
  characters or nil.  The value nil means display the character in the
  default fashion; otherwise, the characters from the vector are delivered
  to the screen instead of the original character.

  For example, (aset buffer-display-table ?X [?Y]) tells Emacs
  to display a capital Y instead of each X character.

  In addition, a char-table has six extra slots to control the display of:

    the end of a truncated screen line (extra-slot 0, a single character);
    the end of a continued line (extra-slot 1, a single character);
    the escape character used to display character codes in octal
      (extra-slot 2, a single character);
    the character used as an arrow for control characters (extra-slot 3,
      a single character);
    the decoration indicating the presence of invisible lines (extra-slot 4,
      a vector of characters);
    the character used to draw the border between side-by-side windows
      (extra-slot 5, a single character).

  See also the functions `display-table-slot' and `set-display-table-slot'.")

(defvar indicate-buffer-boundaries nil
  "*Visually indicate buffer boundaries and scrolling.
  If non-nil, the first and last line of the buffer are marked in the fringe
  of a window on window-systems with angle bitmaps, or if the window can be
  scrolled, the top and bottom line of the window are marked with up and down
  arrow bitmaps.

  If value is a symbol `left' or `right', both angle and arrow bitmaps
  are displayed in the left or right fringe, resp.  Any other value
  that doesn't look like an alist means display the angle bitmaps in
  the left fringe but no arrows.

  You can exercise more precise control by using an alist as the
  value.  Each alist element (INDICATOR . POSITION) specifies
  where to show one of the indicators.  INDICATOR is one of `top',
  `bottom', `up', `down', or t, which specifies the default position,
  and POSITION is one of `left', `right', or nil, meaning do not show
  this indicator.

  For example, ((top . left) (t . right)) places the top angle bitmap in
  left fringe, the bottom angle bitmap in right fringe, and both arrow
  bitmaps in right fringe.  To show just the angle bitmaps in the left
  fringe, but no arrow bitmaps, use ((top .  left) (bottom . left)).

  You can customize this variable.")

(defvar default-left-margin-width nil
  "Default value of `left-margin-width' for buffers that don't override it.
  This is the same as (default-value 'left-margin-width).")

(defvar default-directory (System/getProperty "user.dir")
  "Name of default directory of current buffer.  Should end with slash.
  To interactively change the default directory, use command `cd'.")

(defvar buffer-read-only nil
  "Non-nil if this buffer is read-only.")

(defvar major-mode 'fundamental-mode
  "Symbol for current buffer's major mode.
  The default value (normally `fundamental-mode') affects new buffers.
  A value of nil means to use the current buffer's major mode, provided
  it is not marked as \"special\".

  When a mode is used by default, `find-file' switches to it before it
  reads the contents into the buffer and before it finishes setting up
  the buffer.  Thus, the mode and its hooks should not expect certain
  variables such as `buffer-read-only' and `buffer-file-coding-system'
  to be set up.

  You can customize this variable.")

(defvar fill-column 70
  "*Column beyond which automatic line-wrapping should happen.
  Interactively, you can set the buffer local value using C-x f.

  You can customize this variable.")

(defvar tab-width 8
  "*Distance between tab stops (for display of tab characters), in columns.
  This should be an integer greater than zero.

  You can customize this variable.")

(defvar default-cursor-in-non-selected-windows nil
  "Default value of `cursor-in-non-selected-windows'.
  This is the same as (default-value 'cursor-in-non-selected-windows).")

(defvar default-case-fold-search nil
  "Default value of `case-fold-search' for buffers that don't override it.
  This is the same as (default-value 'case-fold-search).")

(defvar buffer-saved-size 0
  "Length of current buffer when last read in, saved or auto-saved.
  0 initially.
  -1 means auto-saving turned off until next real save.

  If you set this to -2, that means don't turn off auto-saving in this buffer
  if its text size shrinks.   If you use `buffer-swap-text' on a buffer,
  you probably should set this to -2 in that buffer.")

(defvar left-fringe-width nil
  "*Width of this buffer's left fringe (in pixels).
  A value of 0 means no left fringe is shown in this buffer's window.
  A value of nil means to use the left fringe width from the window's frame.")

(defvar buffer-file-coding-system nil
  "Coding system to be used for encoding the buffer contents on saving.
  This variable applies to saving the buffer, and also to `write-region'
  and other functions that use `write-region'.
  It does not apply to sending output to subprocesses, however.

  If this is nil, the buffer is saved without any code conversion
  unless some coding system is specified in `file-coding-system-alist'
  for the buffer file.

  If the text to be saved cannot be encoded as specified by this variable,
  an alternative encoding is selected by `select-safe-coding-system', which see.

  The variable `coding-system-for-write', if non-nil, overrides this variable.

  This variable is never applied to a way of decoding a file while reading it.")

(defvar left-margin 0
  "*Column for the default `indent-line-function' to indent to.
  Linefeed indents to this column in Fundamental mode.

  You can customize this variable.")

(defvar default-scroll-up-aggressively nil
  "Default value of `scroll-up-aggressively'.
  This value applies in buffers that don't have their own local values.
  This is the same as (default-value 'scroll-up-aggressively).")

(defvar default-abbrev-mode nil
  "Default value of `abbrev-mode' for buffers that do not override it.
  This is the same as (default-value 'abbrev-mode).")

(defvar buffer-backed-up nil
  "Non-nil if this buffer's file has been backed up.
  Backing up is done before the first time the file is saved.")

(defvar after-change-functions nil
  "List of functions to call after each text change.
  Three arguments are passed to each function: the positions of
  the beginning and end of the range of changed text,
  and the length in bytes of the pre-change text replaced by that range.
  (For an insertion, the pre-change length is zero;
  for a deletion, that length is the number of bytes deleted,
  and the post-change beginning and end are at the same place.)

  Buffer changes made while executing the `after-change-functions'
  don't call any before-change or after-change functions.
  That's because `inhibit-modification-hooks' is temporarily set non-nil.

  If an unhandled error happens in running these functions,
  the variable's value remains nil.  That prevents the error
  from happening repeatedly and making Emacs nonfunctional.")

(defvar default-fringe-indicator-alist nil
  "Default value of `fringe-indicator-alist' for buffers that don't override it.
  This is the same as (default-value 'fringe-indicator-alist').")

(defvar default-header-line-format nil
  "Default value of `header-line-format' for buffers that don't override it.
  This is the same as (default-value 'header-line-format).")

(defvar buffer-file-format nil
  "List of formats to use when saving this buffer.
  Formats are defined by `format-alist'.  This variable is
  set when a file is visited.")

(defvar header-line-format nil
  "Analogous to `mode-line-format', but controls the header line.
  The header line appears, optionally, at the top of a window;
  the mode line appears at the bottom.")

(defvar buffer-auto-save-file-name nil
  "Name of file for auto-saving current buffer.
  If it is nil, that means don't auto-save this buffer.")

(defvar default-truncate-lines nil
  "Default value of `truncate-lines' for buffers that do not override it.
  This is the same as (default-value 'truncate-lines).")

(defvar default-vertical-scroll-bar nil
  "Default value of `vertical-scroll-bar' for buffers that don't override it.
  This is the same as (default-value 'vertical-scroll-bar).")

(defvar buffer-list-update-hook nil
  "Hook run when the buffer list changes.
  Functions running this hook are `get-buffer-create',
  `make-indirect-buffer', `rename-buffer', `kill-buffer',
  and `bury-buffer-internal'.")

(defvar truncate-lines nil
  "*Non-nil means do not display continuation lines.
  Instead, give each line of text just one screen line.

  Note that this is overridden by the variable
  `truncate-partial-width-windows' if that variable is non-nil
  and this buffer is not full-frame width.

  Minibuffers set this variable to nil.

  You can customize this variable.")

(defvar buffer-display-time nil
  "Time stamp updated each time this buffer is displayed in a window.
  The function `set-window-buffer' updates this variable
  to the value obtained by calling `current-time'.
  If the buffer has never been shown in a window, the value is nil.")

(defvar indicate-empty-lines nil
  "*Visually indicate empty lines after the buffer end.
  If non-nil, a bitmap is displayed in the left fringe of a window on
  window-systems.

  You can customize this variable.")

(defvar ctl-arrow true
  "*Non-nil means display control chars with uparrow.
  A value of nil means use backslash and octal digits.
  This variable does not apply to characters whose display is specified
  in the current display table (if there is one).

  You can customize this variable.")

(defvar case-fold-search true
  "*Non-nil if searches and matches should ignore case.

  You can customize this variable.")

(defvar default-indicate-buffer-boundaries nil
  "Default value of `indicate-buffer-boundaries' for buffers that don't override it.
  This is the same as (default-value 'indicate-buffer-boundaries).")

(defvar line-spacing nil
  "Additional space to put between lines when displaying a buffer.
  The space is measured in pixels, and put below lines on graphic displays,
  see `display-graphic-p'.
  If value is a floating point number, it specifies the spacing relative
  to the default frame line height.  A value of nil means add no extra space.

  You can customize this variable.")

(defvar enable-multibyte-characters true
  "Non-nil means the buffer contents are regarded as multi-byte characters.
  Otherwise they are regarded as unibyte.  This affects the display,
  file I/O and the behavior of various editing commands.

  This variable is buffer-local but you cannot set it directly;
  use the function `set-buffer-multibyte' to change a buffer's representation.
  See also Info node `(elisp)Text Representations'.")

(defvar left-margin-width 0
  "*Width of left marginal area for display of a buffer.
  A value of nil means no marginal area.")

(defvar first-change-hook nil
  "A list of functions to call before changing a buffer which is unmodified.
  The functions are run using the `run-hooks' function.")

(defvar fringes-outside-margins nil
  "*Non-nil means to display fringes outside display margins.
  A value of nil means to display fringes between margins and buffer text.")

(defvar abbrev-mode nil
  "Non-nil if Abbrev mode is enabled.
  Use the command `abbrev-mode' to change this variable.")

(defvar scroll-up-aggressively nil
  "How far to scroll windows upward.
  If you move point off the bottom, the window scrolls automatically.
  This variable controls how far it scrolls.  The value nil, the default,
  means scroll to center point.  A fraction means scroll to put point
  that fraction of the window's height from the bottom of the window.
  When the value is 0.0, point goes at the bottom line, which in the
  simple case that you moved off with C-f means scrolling just one line.
  1.0 means point goes at the top, so that in that simple case, the
  window scrolls by a full window height.  Meaningful values are
  between 0.0 and 1.0, inclusive.

  You can customize this variable.")

(defvar default-fill-column nil
  "Default value of `fill-column' for buffers that do not override it.
  This is the same as (default-value 'fill-column).")

(defvar fringe-indicator-alist nil
  "*Mapping from logical to physical fringe indicator bitmaps.
  The value is an alist where each element (INDICATOR . BITMAPS)
  specifies the fringe bitmaps used to display a specific logical
  fringe indicator.

  INDICATOR specifies the logical indicator type which is one of the
  following symbols: `truncation' , `continuation', `overlay-arrow',
  `top', `bottom', `top-bottom', `up', `down', empty-line', or `unknown'.

  BITMAPS is a list of symbols (LEFT RIGHT [LEFT1 RIGHT1]) which specifies
  the actual bitmap shown in the left or right fringe for the logical
  indicator.  LEFT and RIGHT are the bitmaps shown in the left and/or
  right fringe for the specific indicator.  The LEFT1 or RIGHT1 bitmaps
  are used only for the `bottom' and `top-bottom' indicators when the
  last (only) line has no final newline.  BITMAPS may also be a single
  symbol which is used in both left and right fringes.")

(defvar default-scroll-down-aggressively nil
  "Default value of `scroll-down-aggressively'.
  This value applies in buffers that don't have their own local values.
  This is the same as (default-value 'scroll-down-aggressively).")

(defvar default-tab-width nil
  "Default value of `tab-width' for buffers that do not override it.
  This is the same as (default-value 'tab-width).")

(defvar default-left-fringe-width nil
  "Default value of `left-fringe-width' for buffers that don't override it.
  This is the same as (default-value 'left-fringe-width).")

(defvar buffer-file-name nil
  "Name of file visited in current buffer, or nil if not visiting a file.")

(defvar default-buffer-file-coding-system nil
  "Default value of `buffer-file-coding-system' for buffers not overriding it.
  This is the same as (default-value 'buffer-file-coding-system).")

(defvar cursor-in-non-selected-windows true
  "*Non-nil means show a cursor in non-selected windows.
  If nil, only shows a cursor in the selected window.
  If t, displays a cursor related to the usual cursor type
  (a solid box becomes hollow, a bar becomes a narrower bar).
  You can also specify the cursor type as in the `cursor-type' variable.
  Use Custom to set this variable and update the display.\"

  You can customize this variable.")

(defvar selective-display nil
  "Non-nil enables selective display.
  An integer N as value means display only lines
  that start with less than N columns of space.
  A value of t means that the character ^M makes itself and
  all the rest of the line invisible; also, when saving the buffer
  in a file, save the ^M as a newline.")

(defvar selective-display-ellipses true
  "Non-nil means display ... on previous line when a line is invisible.

  You can customize this variable.")

(defvar cursor-type true
  "Cursor to use when this buffer is in the selected window.
  Values are interpreted as follows:

    t 		  use the cursor specified for the frame
    nil		  don't display a cursor
    box		  display a filled box cursor
    hollow	  display a hollow box cursor
    bar		  display a vertical bar cursor with default width
    (bar . WIDTH)	  display a vertical bar cursor with width WIDTH
    hbar		  display a horizontal bar cursor with default height
    (hbar . HEIGHT) display a horizontal bar cursor with height HEIGHT
    ANYTHING ELSE	  display a hollow box cursor

  When the buffer is displayed in a non-selected window, the
  cursor's appearance is instead controlled by the variable
  `cursor-in-non-selected-windows'.")

(defvar mark-active nil
  "Non-nil means the mark and region are currently active in this buffer.")

(defvar default-fringe-cursor-alist nil
  "Default value of `fringe-cursor-alist' for buffers that don't override it.
  This is the same as (default-value 'fringe-cursor-alist').")

(defvar inhibit-read-only nil
  "*Non-nil means disregard read-only status of buffers or characters.
  If the value is t, disregard `buffer-read-only' and all `read-only'
  text properties.  If the value is a list, disregard `buffer-read-only'
  and disregard a `read-only' text property if the property value
  is a member of the list.")

(defvar buffer-invisibility-spec nil
  "Invisibility spec of this buffer.
  The default is t, which means that text is invisible
  if it has a non-nil `invisible' property.
  If the value is a list, a text character is invisible if its `invisible'
  property is an element in that list (or is a list with members in common).
  If an element is a cons cell of the form (PROP . ELLIPSIS),
  then characters with property value PROP are invisible,
  and they have an ellipsis as well if ELLIPSIS is non-nil.")

(defvar bidi-display-reordering true
  "Non-nil means reorder bidirectional text for display in the visual order.")

(defvar default-right-margin-width nil
  "Default value of `right-margin-width' for buffers that don't override it.
  This is the same as (default-value 'right-margin-width).")

(defvar local-abbrev-table nil
  "Local (mode-specific) abbrev table of current buffer.")

(defvar default-mode-line-format nil
  "Default value of `mode-line-format' for buffers that don't override it.
  This is the same as (default-value 'mode-line-format).")

(defvar bidi-paragraph-direction nil
  "*If non-nil, forces directionality of text paragraphs in the buffer.

  If this is nil (the default), the direction of each paragraph is
  determined by the first strong directional character of its text.
  The values of `right-to-left' and `left-to-right' override that.
  Any other value is treated as nil.

  This variable has no effect unless the buffer's value of
  `bidi-display-reordering' is non-nil.

  You can customize this variable.")

(def ^:private not-buffer-locals
  '#{kill-buffer-hook before-change-functions after-change-functions
     first-change-hook transient-mark-mode inhibit-read-only
     kill-buffer-query-functions change-major-mode-hook buffer-list-update-hook})

(defn ^:private read-buffer-locals []
  (with-open [r (PushbackReader. (io/reader (io/resource (str (s/replace (ns-name *ns*) "." "/") ".clj"))))]
    (group-by #(keyword (or (re-find #"default" (name %)) "local"))
              (remove not-buffer-locals
                      (map second
                           (filter (every-pred seq? (comp '#{defvar} first))
                                   (take-while (complement nil?)
                                               (repeatedly #(read r false nil)))))))))

(defn ^:private init-buffer-locals []
  (let [{:keys [default local]} (read-buffer-locals)]
    (reset! el/buffer-locals (set local))
    (doseq [d default
            :when (not= 'default-directory d)]
      (let [l (el/global (@el/buffer-locals (symbol (s/replace (name d) "default-" ""))))]
        (.refer (the-ns 'deuce.emacs-lisp.globals) d l)))))

(init-buffer-locals)

(def ^:private buffer-alist (atom {}))
(def ^:dynamic ^:private *current-buffer* nil)

(declare current-buffer set-buffer other-buffer buffer-name
         get-buffer buffer-local-value set-buffer-modified-p)

(defun barf-if-buffer-read-only ()
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  )

(defun overlayp (object)
  "Return t if OBJECT is an overlay."
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
  beg)

(defun buffer-live-p (object)
  "Return non-nil if OBJECT is a buffer which has not been killed.
  Value is nil if OBJECT is not a buffer or if it has been killed."
  (contains? (set (vals @buffer-alist)) object))

(defun restore-buffer-modified-p (flag)
  "Like `set-buffer-modified-p', with a difference concerning redisplay.
  It is not ensured that mode lines will be updated to show the modified
  state of the current buffer.  Use with care."
  (set-buffer-modified-p flag))

(defun buffer-modified-p (&optional buffer)
  "Return t if BUFFER was modified since its file was last read or saved.
  No argument or nil as argument means use current buffer as BUFFER."
  (let [text (.text (or buffer (current-buffer)))
        modiff @(.modiff text)]
    (and modiff (> modiff @(.save-modiff text)))))

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
  (if-not (contains? @buffer-alist name)
    name
    (loop [idx 2]
      (let [name (str name  "<" idx ">")]
        (if (and (contains? @buffer-alist name) (not= ignore name))
          (recur (inc idx))
          name)))))

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

(defn ^:private allocate-marker [insertion-type buffer charpos]
  (Marker. (atom  insertion-type) (atom buffer) (atom charpos)))

;; The eternal battle of how to represent mutable data like pt and name, nested atoms or updates via root buffer-alist?
;; The latter doesn't work properly, save-current-buffer for example allows destructive updates to the current buffer it restores.
(defn ^:private allocate-buffer [name]
  (let [now (System/currentTimeMillis)
        text (BufferText. (StringBuilder.) (atom nil) (atom now) nil)
        own-text text
        pt (atom 1)
        mark (atom nil)
        buffer-locals (atom {})
        buffer (Buffer. own-text text pt (atom name) mark buffer-locals false)]
    (reset! mark (allocate-marker nil buffer @pt))
    buffer))

(defun get-buffer-create (buffer-or-name)
  "Return the buffer specified by BUFFER-OR-NAME, creating a new one if needed.
  If BUFFER-OR-NAME is a string and a live buffer with that name exists,
  return that buffer.  If no such buffer exists, create a new buffer with
  that name and return it.  If BUFFER-OR-NAME starts with a space, the new
  buffer does not keep undo information.

  If BUFFER-OR-NAME is a buffer instead of a string, return it as given,
  even if it is dead.  The return value is never nil."
  (if (data/bufferp buffer-or-name)
    buffer-or-name
    (let [buffer (or (get-buffer buffer-or-name) (allocate-buffer buffer-or-name))]
      (swap! buffer-alist assoc buffer-or-name buffer)
      buffer)))

(defun overlay-start (overlay)
  "Return the position at which OVERLAY starts."
  )

(defun get-buffer (buffer-or-name)
  "Return the buffer named BUFFER-OR-NAME.
  BUFFER-OR-NAME must be either a string or a buffer.  If BUFFER-OR-NAME
  is a string and there is no buffer with that name, return nil.  If
  BUFFER-OR-NAME is a buffer, return it as given."
  (if (data/bufferp buffer-or-name)
    buffer-or-name
    (and (el/check-type 'stringp buffer-or-name)
         (@buffer-alist buffer-or-name))))

(defun make-indirect-buffer (base-buffer name &optional clone)
  "Create and return an indirect buffer for buffer BASE-BUFFER, named NAME.
  BASE-BUFFER should be a live buffer, or the name of an existing buffer.
  NAME should be a string which is not the name of an existing buffer.
  Optional argument CLONE non-nil means preserve BASE-BUFFER's state,
  such as major and minor modes, in the indirect buffer.
  CLONE nil means the indirect buffer's state is reset to default values."
  )

(defun current-buffer ()
  "Return the current buffer as a Lisp object."
  *current-buffer*)

(defun delete-overlay (overlay)
  "Delete the overlay OVERLAY from its buffer."
  )

(defun buffer-base-buffer (&optional buffer)
  "Return the base buffer of indirect buffer BUFFER.
  If BUFFER is not indirect, return nil.
  BUFFER defaults to the current buffer."
  )

(defun rename-buffer (newname &optional unique)
  "Change current buffer's name to NEWNAME (a string).
  If second arg UNIQUE is nil or omitted, it is an error if a
  buffer named NEWNAME already exists.
  If UNIQUE is non-nil, come up with a new name using
  `generate-new-buffer-name'.
  Interactively, you can set UNIQUE with a prefix argument.
  We return the name we actually gave the buffer.
  This does not change the name of the visited file (if any)."
  (let [buffer-exists? (contains? @buffer-alist newname)]
    (if (= newname (buffer-name))
      newname
      (if (and unique buffer-exists?)
        (el/throw* 'error (format "Buffer name `%s' is in use" newname))
        (let [newname (if buffer-exists? (generate-new-buffer-name newname) newname)]
          (swap! buffer-alist dissoc (buffer-name))
          (swap! buffer-alist assoc newname (current-buffer))
          (reset! (.name (current-buffer)) newname))))))

(defun overlay-buffer (overlay)
  "Return the buffer OVERLAY belongs to.
  Return nil if OVERLAY has been deleted."
  )

(defun erase-buffer ()
  "Delete the entire contents of the current buffer.
  Any narrowing restriction in effect (see `narrow-to-region') is removed,
  so the buffer is truly empty after this."
  (let [text (.text (current-buffer))]
    (reset! (.pt (current-buffer)) 1)
    (reset! (.modiff text) (System/currentTimeMillis))
    (.setLength (.beg text) 0)))

(defun kill-all-local-variables ()
  "Switch to Fundamental mode by killing current buffer's local variables.
  Most local variable bindings are eliminated so that the default values
  become effective once more.  Also, the syntax table is set from
  `standard-syntax-table', the local keymap is set to nil,
  and the abbrev table from `fundamental-mode-abbrev-table'.
  This function also forces redisplay of the mode line.

  Every function to select a new major mode starts by
  calling this function.

  As a special exception, local variables whose names have
  a non-nil `permanent-local' property are not eliminated by this function.

  The first thing this function does is run
  the normal hook `change-major-mode-hook'."
  )

(defun overlay-end (overlay)
  "Return the position at which OVERLAY ends."
  )

(defun buffer-name (&optional buffer)
  "Return the name of BUFFER, as a string.
  BUFFER defaults to the current buffer.
  Return nil if BUFFER has been killed."
  @(.name (or (and buffer (el/check-type 'bufferp buffer))
              (current-buffer))))

(defun overlay-put (overlay prop value)
  "Set one property of overlay OVERLAY: give property PROP value VALUE.
  VALUE will be returned."
  )

(defun set-buffer (buffer-or-name)
  "Make buffer BUFFER-OR-NAME current for editing operations.
  BUFFER-OR-NAME may be a buffer or the name of an existing buffer.  See
  also `save-excursion' when you want to make a buffer current
  temporarily.  This function does not display the buffer, so its effect
  ends when the current command terminates.  Use `switch-to-buffer' or
  `pop-to-buffer' to switch buffers permanently."
  ;; This is not correct, should only change the binding, but will do for now.
  (alter-var-root #'*current-buffer*
                  (constantly
                   (or (get-buffer buffer-or-name)
                       (el/throw* 'error (format  "No buffer named %s" buffer-or-name))))))

(defun buffer-enable-undo (&optional buffer)
  "Start keeping undo information for buffer BUFFER.
  No argument or nil as argument means do this for the current buffer."
  )

(defun buffer-list (&optional frame)
  "Return a list of all existing live buffers.
  If the optional arg FRAME is a frame, we return the buffer list in the
  proper order for that frame: the buffers show in FRAME come first,
  followed by the rest of the buffers."
  (cons/maybe-seq (vals @buffer-alist)))

(defun bury-buffer-internal (buffer)
  "Move BUFFER to the end of the buffer list."
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
  No argument or nil as argument means use current buffer as BUFFER."
  (cons/maybe-seq (map #(alloc/cons (key %) (when-let [v (val %)] @v))
                       (merge (zipmap @el/buffer-locals (repeat nil))
                              @(.local-var-alist (or buffer (current-buffer)))))))

(defun kill-buffer (&optional buffer-or-name)
  "Kill buffer BUFFER-OR-NAME.
  The argument may be a buffer or the name of an existing buffer.
  Argument nil or omitted means kill the current buffer.  Return t if the
  buffer is actually killed, nil otherwise.

  This function calls `replace-buffer-in-windows' for cleaning up all
  windows currently displaying the buffer to be killed.  The functions in
  `kill-buffer-query-functions' are called with the buffer to be killed as
  the current buffer.  If any of them returns nil, the buffer is not
  killed.  The hook `kill-buffer-hook' is run before the buffer is
  actually killed.  The buffer being killed will be current while the hook
  is running.

  Any processes that have this buffer as the `process-buffer' are killed
  with SIGHUP."
  (let [buffer (if (instance? Buffer buffer-or-name)
                 buffer-or-name
                 (or (@buffer-alist buffer-or-name (current-buffer))))]
    (if (or (not buffer)
            (and globals/kill-buffer-query-functions
                 (binding [*current-buffer* buffer]
                   (some nil? (map eval/funcall globals/kill-buffer-query-functions)))))
      false
      (do
        (binding [*current-buffer* buffer]
          (eval/run-hooks 'kill-buffer-hook))
        (swap! buffer-alist dissoc @(.name buffer))
        (set-buffer (other-buffer))
        true))))

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
  "Swap the text between current buffer and BUFFER."
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
  A non-nil FLAG means mark the buffer modified."
  (reset! (.modiff (.text (current-buffer))) (when flag (System/currentTimeMillis))))

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
  (buffer-local-value 'buffer-file-name (or buffer (current-buffer))))

(defun buffer-local-value (variable buffer)
  "Return the value of VARIABLE in BUFFER.
  If VARIABLE does not have a buffer-local binding in BUFFER, the value
  is the default binding of the variable."
  (binding [*current-buffer* buffer]
    (data/symbol-value variable)))

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
  Buffers not visible in windows are preferred to visible buffers, unless
  optional second argument VISIBLE-OK is non-nil.  Ignore the argument
  BUFFER unless it denotes a live buffer.  If the optional third argument
  FRAME is non-nil, use that frame's buffer list instead of the selected
  frame's buffer list.

  The buffer is found by scanning the selected or specified frame's buffer
  list first, followed by the list of all buffers.  If no other buffer
  exists, return the buffer `*scratch*' (creating it if necessary)."
  (or (first (remove #{(or (and buffer (el/check-type 'bufferp buffer))
                           (current-buffer))} (buffer-list)))
      (get-buffer-create "*scratch*")))

(defun overlays-at (pos)
  "Return a list of the overlays that contain the character at POS."
  )

(defun set-buffer-major-mode (buffer)
  "Set an appropriate major mode for BUFFER.
  For the *scratch* buffer, use `initial-major-mode', otherwise choose a mode
  according to `default-major-mode'.
  Use this function before selecting the buffer, since it may need to inspect
  the current buffer's major mode."
  (when-let [mode (if (= "*scratch*" (buffer-name buffer))
                    (data/symbol-value 'initial-major-mode)
                    (or (data/default-value 'major-mode)
                        (data/symbol-value 'major-mode)))]
    (binding [current-buffer buffer]
      (eval/funcall mode))))

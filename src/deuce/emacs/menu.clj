(ns deuce.emacs.menu
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c])
  (:refer-clojure :exclude []))

(defun x-popup-menu (position menu)
  "Pop up a deck-of-cards menu and return user's selection.
  POSITION is a position specification.  This is either a mouse button event
  or a list ((XOFFSET YOFFSET) WINDOW)
  where XOFFSET and YOFFSET are positions in pixels from the top left
  corner of WINDOW.  (WINDOW may be a window or a frame object.)
  This controls the position of the top left of the menu as a whole.
  If POSITION is t, it means to use the current mouse position.

  MENU is a specifier for a menu.  For the simplest case, MENU is a keymap.
  The menu items come from key bindings that have a menu string as well as
  a definition; actually, the \"definition\" in such a key binding looks like
  (STRING . REAL-DEFINITION).  To give the menu a title, put a string into
  the keymap as a top-level element.

  If REAL-DEFINITION is nil, that puts a nonselectable string in the menu.
  Otherwise, REAL-DEFINITION should be a valid key binding definition.

  You can also use a list of keymaps as MENU.
    Then each keymap makes a separate pane.

  When MENU is a keymap or a list of keymaps, the return value is the
  list of events corresponding to the user's choice. Note that
  `x-popup-menu' does not actually execute the command bound to that
  sequence of events.

  Alternatively, you can specify a menu of multiple panes
    with a list of the form (TITLE PANE1 PANE2...),
  where each pane is a list of form (TITLE ITEM1 ITEM2...).
  Each ITEM is normally a cons cell (STRING . VALUE);
  but a string can appear as an item--that makes a nonselectable line
  in the menu.
  With this form of menu, the return value is VALUE from the chosen item.

  If POSITION is nil, don't display the menu at all, just precalculate the
  cached information about equivalent key sequences.

  If the user gets rid of the menu without making a valid choice, for
  instance by clicking the mouse away from a valid choice or by typing
  keyboard input, then this normally results in a quit and
  `x-popup-menu' does not return.  But if POSITION is a mouse button
  event (indicating that the user invoked the menu with the mouse) then
  no quit occurs and `x-popup-menu' returns nil."
  )

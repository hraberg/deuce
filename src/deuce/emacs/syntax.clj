(ns deuce.emacs.syntax
  (:use [deuce.emacs-lisp :only (defun defvar)])
  (:require [clojure.core :as c])
  (:refer-clojure :exclude []))

(defvar words-include-escapes nil
  "Non-nil means `forward-word', etc., should treat escape chars part of words.

  You can customize this variable.")

(defvar parse-sexp-lookup-properties nil
  "Non-nil means `forward-sexp', etc., obey `syntax-table' property.
  Otherwise, that text property is simply ignored.
  See the info node `(elisp)Syntax Properties' for a description of the
  `syntax-table' property.")

(defvar multibyte-syntax-as-symbol nil
  "Non-nil means `scan-sexps' treats all multibyte characters as symbol.")

(defvar parse-sexp-ignore-comments nil
  "Non-nil means `forward-sexp', etc., should treat comments as whitespace.

  You can customize this variable.")

(defvar find-word-boundary-function-table nil
  "Char table of functions to search for the word boundary.
  Each function is called with two arguments; POS and LIMIT.
  POS and LIMIT are character positions in the current buffer.

  If POS is less than LIMIT, POS is at the first character of a word,
  and the return value of a function is a position after the last
  character of that word.

  If POS is not less than LIMIT, POS is at the last character of a word,
  and the return value of a function is a position at the first
  character of that word.

  In both cases, LIMIT bounds the search.")

(defvar open-paren-in-column-0-is-defun-start nil
  "*Non-nil means an open paren in column 0 denotes the start of a defun.

  You can customize this variable.")

(defun standard-syntax-table ()
  "Return the standard syntax table.
  This is the one used for new buffers."
  )

(defun parse-partial-sexp (from to &optional targetdepth stopbefore oldstate commentstop)
  "Parse Lisp syntax starting at FROM until TO; return status of parse at TO.
  Parsing stops at TO or when certain criteria are met;
   point is set to where parsing stops.
  If fifth arg OLDSTATE is omitted or nil,
   parsing assumes that FROM is the beginning of a function.
  Value is a list of elements describing final state of parsing:
   0. depth in parens.
   1. character address of start of innermost containing list; nil if none.
   2. character address of start of last complete sexp terminated.
   3. non-nil if inside a string.
      (it is the character that will terminate the string,
       or t if the string should be terminated by a generic string delimiter.)
   4. nil if outside a comment, t if inside a non-nestable comment,
      else an integer (the current comment nesting).
   5. t if following a quote character.
   6. the minimum paren-depth encountered during this scan.
   7. style of comment, if any.
   8. character address of start of comment or string; nil if not in one.
   9. Intermediate data for continuation of parsing (subject to change).
  If third arg TARGETDEPTH is non-nil, parsing stops if the depth
  in parentheses becomes equal to TARGETDEPTH.
  Fourth arg STOPBEFORE non-nil means stop when come to
   any character that starts a sexp.
  Fifth arg OLDSTATE is a list like what this function returns.
   It is used to initialize the state of the parse.  Elements number 1, 2, 6
   and 8 are ignored.
  Sixth arg COMMENTSTOP non-nil means stop at the start of a comment.
   If it is symbol `syntax-table', stop after the start of a comment or a
   string, or after end of a comment or a string."
  )

(defun scan-sexps (from count)
  "Scan from character number FROM by COUNT balanced expressions.
  If COUNT is negative, scan backwards.
  Returns the character number of the position thus found.

  Comments are ignored if `parse-sexp-ignore-comments' is non-nil.

  If the beginning or end of (the accessible part of) the buffer is reached
  in the middle of a parenthetical grouping, an error is signaled.
  If the beginning or end is reached between groupings
  but before count is used up, nil is returned."
  )

(defun syntax-table-p (object)
  "Return t if OBJECT is a syntax table.
  Currently, any char-table counts as a syntax table."
  )

(defun forward-word (&optional arg)
  "Move point forward ARG words (backward if ARG is negative).
  Normally returns t.
  If an edge of the buffer or a field boundary is reached, point is left there
  and the function returns nil.  Field boundaries are not noticed if
  `inhibit-field-text-motion' is non-nil."
  )

(defun scan-lists (from count depth)
  "Scan from character number FROM by COUNT lists.
  Scan forward if COUNT is positive, backward if COUNT is negative.
  Return the character number of the position thus found.

  A \"list\", in this context, refers to a balanced parenthetical
  grouping, as determined by the syntax table.

  If DEPTH is nonzero, treat that as the nesting depth of the starting
  point (i.e. the starting point is DEPTH parentheses deep).  This
  function scans over parentheses until the depth goes to zero COUNT
  times.  Hence, positive DEPTH moves out that number of levels of
  parentheses, while negative DEPTH moves to a deeper level.

  Comments are ignored if `parse-sexp-ignore-comments' is non-nil.

  If we reach the beginning or end of the accessible part of the buffer
  before we have scanned over COUNT lists, return nil if the depth at
  that point is zero, and signal a error if the depth is nonzero."
  )

(defun skip-chars-backward (string &optional lim)
  "Move point backward, stopping after a char not in STRING, or at pos LIM.
  See `skip-chars-forward' for details.
  Returns the distance traveled, either zero or negative."
  )

(defun backward-prefix-chars ()
  "Move point backward over any number of chars with prefix syntax.
  This includes chars with \"quote\" or \"prefix\" syntax (' or p)."
  )

(defun copy-syntax-table (&optional table)
  "Construct a new syntax table and return it.
  It is a copy of the TABLE, which defaults to the standard syntax table."
  )

(defun syntax-table ()
  "Return the current syntax table.
  This is the one specified by the current buffer."
  )

(defun skip-syntax-backward (syntax &optional lim)
  "Move point backward across chars in specified syntax classes.
  SYNTAX is a string of syntax code characters.
  Stop on reaching a char whose syntax is not in SYNTAX, or at position LIM.
  If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
  This function returns the distance traveled, either zero or negative."
  )

(defun forward-comment (count)
  "Move forward across up to COUNT comments.  If COUNT is negative, move backward.
  Stop scanning if we find something other than a comment or whitespace.
  Set point to where scanning stops.
  If COUNT comments are found as expected, with nothing except whitespace
  between them, return t; otherwise return nil."
  )

(defun skip-syntax-forward (syntax &optional lim)
  "Move point forward across chars in specified syntax classes.
  SYNTAX is a string of syntax code characters.
  Stop before a char whose syntax is not in SYNTAX, or at position LIM.
  If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
  This function returns the distance traveled, either zero or positive."
  )

(defun set-syntax-table (table)
  "Select a new syntax table for the current buffer.
  One argument, a syntax table."
  )

(defun modify-syntax-entry (char newentry &optional syntax-table)
  "Set syntax for character CHAR according to string NEWENTRY.
  The syntax is changed only for table SYNTAX-TABLE, which defaults to
   the current buffer's syntax table.
  CHAR may be a cons (MIN . MAX), in which case, syntaxes of all characters
  in the range MIN to MAX are changed.
  The first character of NEWENTRY should be one of the following:
    Space or -  whitespace syntax.    w   word constituent.
    _           symbol constituent.   .   punctuation.
    (           open-parenthesis.     )   close-parenthesis.
    \"           string quote.         \\   escape.
    $           paired delimiter.     '   expression quote or prefix operator.
    <           comment starter.      >   comment ender.
    /           character-quote.      @   inherit from `standard-syntax-table'.
    |           generic string fence. !   generic comment fence.

  Only single-character comment start and end sequences are represented thus.
  Two-character sequences are represented as described below.
  The second character of NEWENTRY is the matching parenthesis,
   used only if the first character is `(' or `)'.
  Any additional characters are flags.
  Defined flags are the characters 1, 2, 3, 4, b, p, and n.
   1 means CHAR is the start of a two-char comment start sequence.
   2 means CHAR is the second character of such a sequence.
   3 means CHAR is the start of a two-char comment end sequence.
   4 means CHAR is the second character of such a sequence.

  There can be several orthogonal comment sequences.  This is to support
  language modes such as C++.  By default, all comment sequences are of style
  a, but you can set the comment sequence style to b (on the second character
  of a comment-start, and the first character of a comment-end sequence) and/or
  c (on any of its chars) using this flag:
   b means CHAR is part of comment sequence b.
   c means CHAR is part of comment sequence c.
   n means CHAR is part of a nestable comment sequence.

   p means CHAR is a prefix character for `backward-prefix-chars';
     such characters are treated as whitespace when they occur
     between expressions."
  )

(defun matching-paren (character)
  "Return the matching parenthesis of CHARACTER, or nil if none."
  )

(defun skip-chars-forward (string &optional lim)
  "Move point forward, stopping before a char not in STRING, or at pos LIM.
  STRING is like the inside of a `[...]' in a regular expression
  except that `]' is never special and `\\' quotes `^', `-' or `\\'
   (but not at the end of a range; quoting is never needed there).
  Thus, with arg \"a-zA-Z\", this skips letters stopping before first nonletter.
  With arg \"^a-zA-Z\", skips nonletters stopping before first letter.
  Char classes, e.g. `[:alpha:]', are supported.

  Returns the distance traveled, either zero or positive."
  )

(defun char-syntax (character)
  "Return the syntax code of CHARACTER, described by a character.
  For example, if CHARACTER is a word constituent, the
  character `w' (119) is returned.
  The characters that correspond to various syntax codes
  are listed in the documentation of `modify-syntax-entry'."
  )

(defun string-to-syntax (string)
  "Convert a syntax specification STRING into syntax cell form.
  STRING should be a string as it is allowed as argument of
  `modify-syntax-entry'.  Value is the equivalent cons cell
  (CODE . MATCHING-CHAR) that can be used as value of a `syntax-table'
  text property."
  )

(defun internal-describe-syntax-value (syntax)
  "Insert a description of the internal syntax description SYNTAX at point."
  )

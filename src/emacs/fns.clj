(ns emacs.fns (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun provide (feature &optional subfeatures)
  "Announce that FEATURE is a feature of the current Emacs.
  The optional argument SUBFEATURES should be a list of symbols listing
  particular subfeatures supported in this version of FEATURE."
  )

(defun widget-get (widget property)
  "In WIDGET, get the value of PROPERTY.
  The value could either be specified when the widget was created, or
  later with `widget-put'."
  )

(defun string-as-multibyte (string)
  "Return a multibyte string with the same individual bytes as STRING.
  If STRING is multibyte, the result is STRING itself.
  Otherwise it is a newly created string, with no text properties."
  )

(defun lax-plist-put (plist prop val)
  "Change value in PLIST of PROP to VAL, comparing with `equal'.
  PLIST is a property list, which is a list of the form
  (PROP1 VALUE1 PROP2 VALUE2 ...).  PROP and VAL are any objects.
  If PROP is already a property on the list, its value is set to VAL,
  otherwise the new PROP VAL pair is added.  The new plist is returned;
  use `(setq x (lax-plist-put x prop val))' to be sure to use the new value.
  The PLIST is modified by side effects."
  )

(defun safe-length (list)
  "Return the length of a list, but avoid error or infinite loop.
  This function never gets an error.  If LIST is not really a list,
  it returns 0.  If LIST is circular, it returns a finite value
  which is at least the number of distinct elements."
  )

(defun member (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
  The value is actually the tail of LIST whose car is ELT."
  )

(defun copy-hash-table (table)
  "Return a copy of hash table TABLE."
  )

(defun append (&rest sequences)
  "Concatenate all the arguments and make the result a list.
  The result is a list whose elements are the elements of all the arguments.
  Each argument may be a list, vector or string.
  The last argument is not copied, just used as the tail of the new list."
  )

(defun mapconcat (function sequence separator)
  "Apply FUNCTION to each element of SEQUENCE, and concat the results as strings.
  In between each pair of results, stick in SEPARATOR.  Thus, \" \" as
  SEPARATOR results in spaces between the values returned by FUNCTION.
  SEQUENCE may be a list, a vector, a bool-vector, or a string."
  )

(defun compare-strings (str1 start1 end1 str2 start2 end2 &optional ignore-case)
  "Compare the contents of two strings, converting to multibyte if needed.
  In string STR1, skip the first START1 characters and stop at END1.
  In string STR2, skip the first START2 characters and stop at END2.
  END1 and END2 default to the full lengths of the respective strings."
  )

(defun copy-alist (alist)
  "Return a copy of ALIST.
  This is an alist which represents the same mapping from objects to objects,
  but does not share the alist structure with ALIST.
  The objects mapped (cars and cdrs of elements of the alist)
  are shared, however.
  Elements of ALIST that are not conses are also shared."
  )

(defun copy-sequence (arg)
  "Return a copy of a list, vector, string or char-table.
  The elements of a list or vector are not copied; they are shared
  with the original."
  )

(defun string-as-unibyte (string)
  "Return a unibyte string with the same individual bytes as STRING.
  If STRING is unibyte, the result is STRING itself.
  Otherwise it is a newly created string, with no text properties.
  If STRING is multibyte and contains a character of charset
  `eight-bit', it is converted to the corresponding single byte."
  )

(defun sxhash (obj)
  "Compute a hash code for OBJ and return it as integer."
  )

(defun md5 (object &optional start end coding-system noerror)
  "Return MD5 message digest of OBJECT, a buffer or string."
  )

(defun widget-apply (widget property &rest args)
  "Apply the value of WIDGET's PROPERTY to the widget itself.
  ARGS are passed as extra arguments to the function."
  )

(defun hash-table-p (obj)
  "Return t if OBJ is a Lisp hash table object."
  )

(defun delete (elt seq)
  "Delete by side effect any occurrences of ELT as a member of SEQ.
  SEQ must be a list, a vector, or a string.
  The modified SEQ is returned.  Comparison is done with `equal'.
  If SEQ is not a list, or the first member of SEQ is ELT, deleting it
  is not a side effect; it is simply using a different sequence.
  Therefore, write `(setq foo (delete element foo))'
  to be sure of changing the value of `foo'."
  )

(defun locale-info (item)
  "Access locale data ITEM for the current C locale, if available.
  ITEM should be one of the following:"
  )

(defun string-to-multibyte (string)
  "Return a multibyte string with the same individual chars as STRING.
  If STRING is multibyte, the result is STRING itself.
  Otherwise it is a newly created string, with no text properties."
  )

(defun eql (obj1 obj2)
  "Return t if the two args are the same Lisp object.
  Floating-point numbers of equal value are `eql', but they may not be `eq'."
  )

(defun plist-get (plist prop)
  "Extract a value from a property list.
  PLIST is a property list, which is a list of the form
  (PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
  corresponding to the given PROP, or nil if PROP is not one of the
  properties on the list.  This function never signals an error."
  )

(defun elt (sequence n)
  "Return element of SEQUENCE at index N."
  )

(defun base64-encode-string (string &optional no-line-break)
  "Base64-encode STRING and return the result.
  Optional second argument NO-LINE-BREAK means do not break long lines
  into shorter lines."
  )

(defun equal-including-properties (o1 o2)
  "Return t if two Lisp objects have similar structure and contents.
  This is like `equal' except that it compares the text properties
  of strings.  (`equal' ignores text properties.)"
  )

(defun substring-no-properties (string &optional from to)
  "Return a substring of STRING, without text properties.
  It starts at index FROM and ends before TO.
  TO may be nil or omitted; then the substring runs to the end of STRING.
  If FROM is nil or omitted, the substring starts at the beginning of STRING.
  If FROM or TO is negative, it counts from the end."
  )

(defun random (&optional limit)
  "Return a pseudo-random number.
  All integers representable in Lisp are equally likely.
    On most systems, this is 29 bits' worth.
  With positive integer LIMIT, return random number in interval [0,LIMIT).
  With argument t, set the random number seed from the current time and pid.
  Other values of LIMIT are ignored."
  )

(defun concat (&rest sequences)
  "Concatenate all the arguments and make the result a string.
  The result is a string whose elements are the elements of all the arguments.
  Each argument may be a string or a list or vector of characters (integers)."
  )

(defun string-bytes (string)
  "Return the number of bytes in STRING.
  If STRING is multibyte, this may be greater than the length of STRING."
  )

(defun assoc (key list)
  "Return non-nil if KEY is `equal' to the car of an element of LIST.
  The value is actually the first element of LIST whose car equals KEY."
  )

(defun remhash (key table)
  "Remove KEY from TABLE."
  )

(defun yes-or-no-p (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes.
  Takes one argument, which is the string to display to ask the question.
  It should end in a space; `yes-or-no-p' adds `(yes or no) ' to it.
  The user must confirm the answer with RET,
  and can edit it until it has been confirmed."
  )

(defun hash-table-count (table)
  "Return the number of elements in TABLE."
  )

(defun clear-string (string)
  "Clear the contents of STRING.
  This makes STRING unibyte and may change its length."
  )

(defun delq (elt list)
  "Delete by side effect any occurrences of ELT as a member of LIST.
  The modified LIST is returned.  Comparison is done with `eq'.
  If the first member of LIST is ELT, there is no way to remove it by side effect;
  therefore, write `(setq foo (delq element foo))'
  to be sure of changing the value of `foo'."
  )

(defun assq (key list)
  "Return non-nil if KEY is `eq' to the car of an element of LIST.
  The value is actually the first element of LIST whose car is KEY.
  Elements of LIST that are not conses are ignored."
  )

(defun string-make-multibyte (string)
  "Return the multibyte equivalent of STRING.
  If STRING is unibyte and contains non-ASCII characters, the function
  `unibyte-char-to-multibyte' is used to convert each unibyte character
  to a multibyte character.  In this case, the returned string is a
  newly created string with no text properties.  If STRING is multibyte
  or entirely ASCII, it is returned unchanged.  In particular, when
  STRING is unibyte and entirely ASCII, the returned string is unibyte.
  (When the characters are all ASCII, Emacs primitives will treat the
  string the same way whether it is unibyte or multibyte.)"
  )

(defun string-equal (s1 s2)
  "Return t if two strings have identical contents.
  Case is significant, but text properties are ignored.
  Symbols are also allowed; their print names are used instead."
  )

(defun mapcar (function sequence)
  "Apply FUNCTION to each element of SEQUENCE, and make a list of the results.
  The result is a list just as long as SEQUENCE.
  SEQUENCE may be a list, a vector, a bool-vector, or a string."
  )

(defun fillarray (array item)
  "Store each element of ARRAY with ITEM.
  ARRAY is a vector, string, char-table, or bool-vector."
  )

(defun load-average (&optional use-floats)
  "Return list of 1 minute, 5 minute and 15 minute load averages."
  )

(defun base64-encode-region (beg end &optional no-line-break)
  "Base64-encode the region between BEG and END.
  Return the length of the encoded text.
  Optional third argument NO-LINE-BREAK means do not break long lines
  into shorter lines."
  )

(defun hash-table-weakness (table)
  "Return the weakness of TABLE."
  )

(defun clrhash (table)
  "Clear hash table TABLE and return it."
  )

(defun vconcat (&rest sequences)
  "Concatenate all the arguments and make the result a vector.
  The result is a vector whose elements are the elements of all the arguments.
  Each argument may be a list, vector or string."
  )

(defun make-hash-table (&rest keyword-args)
  "Create and return a new hash table."
  )

(defun rassoc (key list)
  "Return non-nil if KEY is `equal' to the cdr of an element of LIST.
  The value is actually the first element of LIST whose cdr equals KEY."
  )

(defun equal (o1 o2)
  "Return t if two Lisp objects have similar structure and contents.
  They must have the same data type.
  Conses are compared by comparing the cars and the cdrs.
  Vectors and strings are compared element by element.
  Numbers are compared by value, but integers cannot equal floats.
   (Use `=' if you want integers and floats to be able to be equal.)
  Symbols must match exactly."
  )

(defun nreverse (list)
  "Reverse LIST by modifying cdr pointers.
  Return the reversed list."
  )

(defun reverse (list)
  "Reverse LIST, copying.  Return the reversed list.
  See also the function `nreverse', which is used more often."
  )

(defun nthcdr (n list)
  "Take cdr N times on LIST, return the result."
  )

(defun hash-table-rehash-size (table)
  "Return the current rehash size of TABLE."
  )

(defun put (symbol propname value)
  "Store SYMBOL's PROPNAME property with value VALUE.
  It can be retrieved with `(get SYMBOL PROPNAME)'."
  )

(defun base64-decode-region (beg end)
  "Base64-decode the region between BEG and END.
  Return the length of the decoded text.
  If the region can't be decoded, signal an error and don't modify the buffer."
  )

(defun nth (n list)
  "Return the Nth element of LIST.
  N counts from zero.  If LIST is not that long, nil is returned."
  )

(defun string-to-unibyte (string)
  "Return a unibyte string with the same individual chars as STRING.
  If STRING is unibyte, the result is STRING itself.
  Otherwise it is a newly created string, with no text properties,
  where each `eight-bit' character is converted to the corresponding byte.
  If STRING contains a non-ASCII, non-`eight-bit' character,
  an error is signaled."
  )

(defun nconc (&rest lists)
  "Concatenate any number of lists by altering them.
  Only the last argument is not altered, and need not be a list."
  )

(defun length (sequence)
  "Return the length of vector, list or string SEQUENCE.
  A byte-code function object is also allowed.
  If the string contains multibyte characters, this is not necessarily
  the number of bytes in the string; it is the number of characters.
  To get the number of bytes, use `string-bytes'."
  )

(defun memq (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.
  The value is actually the tail of LIST whose car is ELT."
  )

(defun memql (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `eql'.
  The value is actually the tail of LIST whose car is ELT."
  )

(defun gethash (key table &optional dflt)
  "Look up KEY in TABLE and return its associated value.
  If KEY is not found, return DFLT which defaults to nil."
  )

(defun identity (arg)
  "Return the argument unchanged."
  )

(defun define-hash-table-test (name test hash)
  "Define a new hash table test with name NAME, a symbol."
  )

(defun hash-table-size (table)
  "Return the size of TABLE.
  The size can be used as an argument to `make-hash-table' to create
  a hash table than can hold as many elements as TABLE holds
  without need for resizing."
  )

(defun require (feature &optional filename noerror)
  "If feature FEATURE is not loaded, load it from FILENAME.
  If FEATURE is not a member of the list `features', then the feature
  is not loaded; so load the file FILENAME.
  If FILENAME is omitted, the printname of FEATURE is used as the file name,
  and `load' will try to load this name appended with the suffix `.elc' or
  `.el', in that order.  The name without appended suffix will not be used.
  If the optional third argument NOERROR is non-nil,
  then return nil if the file is not found instead of signaling an error.
  Normally the return value is FEATURE.
  The normal messages at start and end of loading FILENAME are suppressed."
  )

(defun get (symbol propname)
  "Return the value of SYMBOL's PROPNAME property.
  This is the last value stored with `(put SYMBOL PROPNAME VALUE)'."
  )

(defun lax-plist-get (plist prop)
  "Extract a value from a property list, comparing with `equal'.
  PLIST is a property list, which is a list of the form
  (PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
  corresponding to the given PROP, or nil if PROP is not
  one of the properties on the list."
  )

(defun maphash (function table)
  "Call FUNCTION for all entries in hash table TABLE.
  FUNCTION is called with two arguments, KEY and VALUE."
  )

(defun rassq (key list)
  "Return non-nil if KEY is `eq' to the cdr of an element of LIST.
  The value is actually the first element of LIST whose cdr is KEY."
  )

(defun string-make-unibyte (string)
  "Return the unibyte equivalent of STRING.
  Multibyte character codes are converted to unibyte according to
  `nonascii-translation-table' or, if that is nil, `nonascii-insert-offset'.
  If the lookup in the translation table fails, this function takes just
  the low 8 bits of each character."
  )

(defun y-or-n-p (prompt)
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".
  Takes one argument, which is the string to display to ask the question.
  It should end in a space; `y-or-n-p' adds `(y or n) ' to it.
  No confirmation of the answer is requested; a single character is enough.
  Also accepts Space to mean yes, or Delete to mean no.  (Actually, it uses
  the bindings in `query-replace-map'; see the documentation of that variable
  for more information.  In this case, the useful bindings are `act', `skip',
  `recenter', and `quit'.)"
  )

(defun string-lessp (s1 s2)
  "Return t if first arg string is less than second in lexicographic order.
  Case is significant.
  Symbols are also allowed; their print names are used instead."
  )

(defun widget-put (widget property value)
  "In WIDGET, set PROPERTY to VALUE.
  The value can later be retrieved with `widget-get'."
  )

(defun substring (string from &optional to)
  "Return a new string whose contents are a substring of STRING.
  The returned string consists of the characters between index FROM
  (inclusive) and index TO (exclusive) of STRING.  FROM and TO are
  zero-indexed: 0 means the first character of STRING.  Negative values
  are counted from the end of STRING.  If TO is nil, the substring runs
  to the end of STRING."
  )

(defun featurep (feature &optional subfeature)
  "Return t if FEATURE is present in this Emacs."
  )

(defun hash-table-rehash-threshold (table)
  "Return the current rehash threshold of TABLE."
  )

(defun plist-put (plist prop val)
  "Change value in PLIST of PROP to VAL.
  PLIST is a property list, which is a list of the form
  (PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol and VAL is any object.
  If PROP is already a property on the list, its value is set to VAL,
  otherwise the new PROP VAL pair is added.  The new plist is returned;
  use `(setq x (plist-put x prop val))' to be sure to use the new value.
  The PLIST is modified by side effects."
  )

(defun puthash (key value table)
  "Associate KEY with VALUE in hash table TABLE.
  If KEY is already present in table, replace its current value with
  VALUE."
  )

(defun hash-table-test (table)
  "Return the test TABLE uses."
  )

(defun mapc (function sequence)
  "Apply FUNCTION to each element of SEQUENCE for side effects only.
  Unlike `mapcar', don't accumulate the results.  Return SEQUENCE.
  SEQUENCE may be a list, a vector, a bool-vector, or a string."
  )

(defun plist-member (plist prop)
  "Return non-nil if PLIST has the property PROP.
  PLIST is a property list, which is a list of the form
  (PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol.
  Unlike `plist-get', this allows you to distinguish between a missing
  property and a property with the value nil.
  The value is actually the tail of PLIST whose car is PROP."
  )

(defun sort (list predicate)
  "Sort LIST, stably, comparing elements using PREDICATE.
  Returns the sorted list.  LIST is modified by side effects.
  PREDICATE is called with two elements of LIST, and should return non-nil
  if the first element should sort before the second."
  )

(defun base64-decode-string (string)
  "Base64-decode STRING and return the result."
  )

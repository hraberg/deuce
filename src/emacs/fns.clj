(ns emacs.fns (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun provide (feature &optional subfeatures)
  "Announce that FEATURE is a feature of the current Emacs.\nThe optional argument SUBFEATURES should be a list of symbols listing\nparticular subfeatures supported in this version of FEATURE.interactive is a special form in `C source code'."
  )

(defun widget-get (widget property)
  "In WIDGET, get the value of PROPERTY.\nThe value could either be specified when the widget was created, or\n"
  )

(defun string-as-multibyte (string)
  "Return a multibyte string with the same individual bytes as STRING.\nIf STRING is multibyte, the result is STRING itself.\nOtherwise it is a newly created string, with no text properties."
  )

(defun lax-plist-put (plist prop val)
  "Change value in PLIST of PROP to VAL, comparing with `equal'.\nPLIST is a property list, which is a list of the form\n(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP and VAL are any objects.\nIf PROP is already a property on the list, its value is set to VAL,\notherwise the new PROP VAL pair is added.  The new plist is returned;\nuse `(setq x (lax-plist-put x prop val))' to be sure to use the new value.\n"
  )

(defun safe-length (list)
  "Return the length of a list, but avoid error or infinite loop.\nThis function never gets an error.  If LIST is not really a list,\nit returns 0.  If LIST is circular, it returns a finite value\nwhich is at least the number of distinct elements.delete-backward-char is an interactive built-in function in `C source\ncode'."
  )

(defun member (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.\n"
  )

(defun copy-hash-table (table)
  )

(defun append (&rest sequences)
  "Concatenate all the arguments and make the result a list.\nThe result is a list whose elements are the elements of all the arguments.\nEach argument may be a list, vector or string.\n"
  )

(defun mapconcat (function sequence separator)
  "Apply FUNCTION to each element of SEQUENCE, and concat the results as strings.\nIn between each pair of results, stick in SEPARATOR.  Thus, \" \" as\nSEPARATOR results in spaces between the values returned by FUNCTION.\n"
  )

(defun compare-strings (str1 start1 end1 str2 start2 end2 &optional ignore-case)
  "Compare the contents of two strings, converting to multibyte if needed.\nIn string STR1, skip the first START1 characters and stop at END1.\nIn string STR2, skip the first START2 characters and stop at END2.\nEND1 and END2 default to the full lengths of the respective strings."
  )

(defun copy-alist (alist)
  "Return a copy of ALIST.\nThis is an alist which represents the same mapping from objects to objects,\nbut does not share the alist structure with ALIST.\nThe objects mapped (cars and cdrs of elements of the alist)\nare shared, however.\n"
  )

(defun copy-sequence (arg)
  "Return a copy of a list, vector, string or char-table.\nThe elements of a list or vector are not copied; they are shared\n"
  )

(defun string-as-unibyte (string)
  "Return a unibyte string with the same individual bytes as STRING.\nIf STRING is unibyte, the result is STRING itself.\nOtherwise it is a newly created string, with no text properties.\nIf STRING is multibyte and contains a character of charset\n"
  )

(defun sxhash (obj)
  "Compute a hash code for OBJ and return it as integer.shrink-window is an interactive built-in function in `C source code'."
  )

(defun md5 (object &optional start end coding-system noerror)
  "Return MD5 message digest of OBJECT, a buffer or string."
  )

(defun widget-apply (widget property &rest args)
  "Apply the value of WIDGET's PROPERTY to the widget itself.\n"
  )

(defun hash-table-p (obj)
  "Return t if OBJ is a Lisp hash table object.handle-switch-frame is an interactive built-in function in `C source\ncode'."
  )

(defun delete (elt seq)
  "Delete by side effect any occurrences of ELT as a member of SEQ.\nSEQ must be a list, a vector, or a string.\nThe modified SEQ is returned.  Comparison is done with `equal'.\nIf SEQ is not a list, or the first member of SEQ is ELT, deleting it\nis not a side effect; it is simply using a different sequence.\nTherefore, write `(setq foo (delete element foo))'\n"
  )

(defun locale-info (item)
  "Access locale data ITEM for the current C locale, if available.\nITEM should be one of the following:"
  )

(defun string-to-multibyte (string)
  "Return a multibyte string with the same individual chars as STRING.\nIf STRING is multibyte, the result is STRING itself.\nOtherwise it is a newly created string, with no text properties."
  )

(defun eql (obj1 obj2)
  "Return t if the two args are the same Lisp object.\nFloating-point numbers of equal value are `eql', but they may not be `eq'.search-backward-regexp is an interactive built-in function in\n`subr.el'."
  )

(defun plist-get (plist prop)
  "Extract a value from a property list.\nPLIST is a property list, which is a list of the form\n(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value\ncorresponding to the given PROP, or nil if PROP is not one of the\nproperties on the list.  This function never signals an error.get-char-property-and-overlay is a built-in function in `C source\ncode'."
  )

(defun elt (sequence n)
  "Return element of SEQUENCE at index N.while is a special form in `C source code'."
  )

(defun base64-encode-string (string &optional no-line-break)
  "Base64-encode STRING and return the result.\nOptional second argument NO-LINE-BREAK means do not break long lines\ninto shorter lines.capitalize-region is an interactive built-in function in `C source\ncode'."
  )

(defun equal-including-properties (o1 o2)
  "Return t if two Lisp objects have similar structure and contents.\nThis is like `equal' except that it compares the text properties\nof strings.  (`equal' ignores text properties.)search-backward is an interactive built-in function in `C source\ncode'."
  )

(defun substring-no-properties (string &optional from to)
  "Return a substring of STRING, without text properties.\nIt starts at index FROM and ends before TO.\nTO may be nil or omitted; then the substring runs to the end of STRING.\nIf FROM is nil or omitted, the substring starts at the beginning of STRING.\nIf FROM or TO is negative, it counts from the end."
  )

(defun random (&optional limit)
  "Return a pseudo-random number.\nAll integers representable in Lisp are equally likely.\n  On most systems, this is 29 bits' worth.\nWith positive integer LIMIT, return random number in interval [0,LIMIT).\nWith argument t, set the random number seed from the current time and pid.\n"
  )

(defun concat (&rest sequences)
  "Concatenate all the arguments and make the result a string.\nThe result is a string whose elements are the elements of all the arguments.\n"
  )

(defun string-bytes (string)
  "Return the number of bytes in STRING.\n"
  )

(defun assoc (key list)
  "Return non-nil if KEY is `equal' to the car of an element of LIST.\n"
  )

(defun remhash (key table)
  )

(defun yes-or-no-p (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes.\nTakes one argument, which is the string to display to ask the question.\nIt should end in a space; `yes-or-no-p' adds `(yes or no) ' to it.\nThe user must confirm the answer with RET,\nand can edit it until it has been confirmed."
  )

(defun hash-table-count (table)
  "Return the number of elements in TABLE.eval-region is an interactive built-in function in `C source code'."
  )

(defun clear-string (string)
  "Clear the contents of STRING.\n"
  )

(defun delq (elt list)
  "Delete by side effect any occurrences of ELT as a member of LIST.\nThe modified LIST is returned.  Comparison is done with `eq'.\nIf the first member of LIST is ELT, there is no way to remove it by side effect;\ntherefore, write `(setq foo (delq element foo))'\n"
  )

(defun assq (key list)
  "Return non-nil if KEY is `eq' to the car of an element of LIST.\nThe value is actually the first element of LIST whose car is KEY.\n"
  )

(defun string-make-multibyte (string)
  "Return the multibyte equivalent of STRING.\nIf STRING is unibyte and contains non-ASCII characters, the function\n`unibyte-char-to-multibyte' is used to convert each unibyte character\nto a multibyte character.  In this case, the returned string is a\nnewly created string with no text properties.  If STRING is multibyte\nor entirely ASCII, it is returned unchanged.  In particular, when\nSTRING is unibyte and entirely ASCII, the returned string is unibyte.\n(When the characters are all ASCII, Emacs primitives will treat the\nstring the same way whether it is unibyte or multibyte.)set-process-inherit-coding-system-flag is a built-in function in `C\nsource code'."
  )

(defun string-equal (s1 s2)
  "Return t if two strings have identical contents.\nCase is significant, but text properties are ignored.\n"
  )

(defun mapcar (function sequence)
  "Apply FUNCTION to each element of SEQUENCE, and make a list of the results.\nThe result is a list just as long as SEQUENCE.\n"
  )

(defun fillarray (array item)
  "Store each element of ARRAY with ITEM.\n"
  )

(defun load-average (&optional use-floats)
  "Return list of 1 minute, 5 minute and 15 minute load averages."
  )

(defun hash-table-weakness (table)
  )

(defun clrhash (table)
  )

(defun vconcat (&rest sequences)
  "Concatenate all the arguments and make the result a vector.\nThe result is a vector whose elements are the elements of all the arguments.\n"
  )

(defun make-hash-table (&rest keyword-args)
  "Create and return a new hash table."
  )

(defun rassoc (key list)
  "Return non-nil if KEY is `equal' to the cdr of an element of LIST.\n"
  )

(defun equal (o1 o2)
  "Return t if two Lisp objects have similar structure and contents.\nThey must have the same data type.\nConses are compared by comparing the cars and the cdrs.\nVectors and strings are compared element by element.\nNumbers are compared by value, but integers cannot equal floats.\n (Use `=' if you want integers and floats to be able to be equal.)\nSymbols must match exactly.internal-merge-in-global-face is a built-in function in `C source\ncode'."
  )

(defun nreverse (list)
  "Reverse LIST by modifying cdr pointers.\n"
  )

(defun reverse (list)
  "Reverse LIST, copying.  Return the reversed list.\n"
  )

(defun nthcdr (n list)
  )

(defun hash-table-rehash-size (table)
  )

(defun put (symbol propname value)
  "Store SYMBOL's PROPNAME property with value VALUE.\n"
  )

(defun nth (n list)
  "Return the Nth element of LIST.\nN counts from zero.  If LIST is not that long, nil is returned.cond is a special form in `C source code'."
  )

(defun string-to-unibyte (string)
  "Return a unibyte string with the same individual chars as STRING.\nIf STRING is unibyte, the result is STRING itself.\nOtherwise it is a newly created string, with no text properties,\nwhere each `eight-bit' character is converted to the corresponding byte.\nIf STRING contains a non-ASCII, non-`eight-bit' character,\n"
  )

(defun nconc (&rest lists)
  "Concatenate any number of lists by altering them.\nOnly the last argument is not altered, and need not be a list.indent-to is an interactive built-in function in `C source code'."
  )

(defun length (sequence)
  "Return the length of vector, list or string SEQUENCE.\nA byte-code function object is also allowed.\nIf the string contains multibyte characters, this is not necessarily\nthe number of bytes in the string; it is the number of characters.\n"
  )

(defun memq (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.\n"
  )

(defun memql (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `eql'.\n"
  )

(defun gethash (key table &optional dflt)
  "Look up KEY in TABLE and return its associated value.\n"
  )

(defun identity (arg)
  )

(defun define-hash-table-test (name test hash)
  "Define a new hash table test with name NAME, a symbol."
  )

(defun hash-table-size (table)
  "Return the size of TABLE.\nThe size can be used as an argument to `make-hash-table' to create\na hash table than can hold as many elements as TABLE holds\n"
  )

(defun require (feature &optional filename noerror)
  "If feature FEATURE is not loaded, load it from FILENAME.\nIf FEATURE is not a member of the list `features', then the feature\nis not loaded; so load the file FILENAME.\nIf FILENAME is omitted, the printname of FEATURE is used as the file name,\nand `load' will try to load this name appended with the suffix `.elc' or\n`.el', in that order.  The name without appended suffix will not be used.\nIf the optional third argument NOERROR is non-nil,\nthen return nil if the file is not found instead of signaling an error.\nNormally the return value is FEATURE.\nThe normal messages at start and end of loading FILENAME are suppressed.execute-extended-command is an interactive built-in function in `C\nsource code'."
  )

(defun get (symbol propname)
  "Return the value of SYMBOL's PROPNAME property.\n"
  )

(defun lax-plist-get (plist prop)
  "Extract a value from a property list, comparing with `equal'.\nPLIST is a property list, which is a list of the form\n(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value\ncorresponding to the given PROP, or nil if PROP is not\n"
  )

(defun maphash (function table)
  "Call FUNCTION for all entries in hash table TABLE.\n"
  )

(defun rassq (key list)
  "Return non-nil if KEY is `eq' to the cdr of an element of LIST.\n"
  )

(defun string-make-unibyte (string)
  "Return the unibyte equivalent of STRING.\nMultibyte character codes are converted to unibyte according to\n`nonascii-translation-table' or, if that is nil, `nonascii-insert-offset'.\nIf the lookup in the translation table fails, this function takes just\n"
  )

(defun y-or-n-p (prompt)
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".\nTakes one argument, which is the string to display to ask the question.\nIt should end in a space; `y-or-n-p' adds `(y or n) ' to it.\nNo confirmation of the answer is requested; a single character is enough.\nAlso accepts Space to mean yes, or Delete to mean no.  (Actually, it uses\nthe bindings in `query-replace-map'; see the documentation of that variable\nfor more information.  In this case, the useful bindings are `act', `skip',\n`recenter', and `quit'.)"
  )

(defun string-lessp (s1 s2)
  "Return t if first arg string is less than second in lexicographic order.\nCase is significant.\n"
  )

(defun widget-put (widget property value)
  "In WIDGET, set PROPERTY to VALUE.\nThe value can later be retrieved with `widget-get'.word-search-backward-lax is an interactive built-in function in `C\nsource code'."
  )

(defun substring (string from &optional to)
  "Return a new string whose contents are a substring of STRING.\nThe returned string consists of the characters between index FROM\n(inclusive) and index TO (exclusive) of STRING.  FROM and TO are\nzero-indexed: 0 means the first character of STRING.  Negative values\nare counted from the end of STRING.  If TO is nil, the substring runs\nto the end of STRING."
  )

(defun featurep (feature &optional subfeature)
  "Return t if FEATURE is present in this Emacs."
  )

(defun hash-table-rehash-threshold (table)
  )

(defun plist-put (plist prop val)
  "Change value in PLIST of PROP to VAL.\nPLIST is a property list, which is a list of the form\n(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol and VAL is any object.\nIf PROP is already a property on the list, its value is set to VAL,\notherwise the new PROP VAL pair is added.  The new plist is returned;\nuse `(setq x (plist-put x prop val))' to be sure to use the new value.\n"
  )

(defun puthash (key value table)
  "Associate KEY with VALUE in hash table TABLE.\nIf KEY is already present in table, replace its current value with\n"
  )

(defun hash-table-test (table)
  )

(defun mapc (function sequence)
  "Apply FUNCTION to each element of SEQUENCE for side effects only.\nUnlike `mapcar', don't accumulate the results.  Return SEQUENCE.\n"
  )

(defun plist-member (plist prop)
  "Return non-nil if PLIST has the property PROP.\nPLIST is a property list, which is a list of the form\n(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol.\nUnlike `plist-get', this allows you to distinguish between a missing\nproperty and a property with the value nil.\n"
  )

(defun sort (list predicate)
  "Sort LIST, stably, comparing elements using PREDICATE.\nReturns the sorted list.  LIST is modified by side effects.\nPREDICATE is called with two elements of LIST, and should return non-nil\n"
  )

(defun base64-decode-string (string)
  )

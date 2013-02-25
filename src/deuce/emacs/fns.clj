(ns deuce.emacs.fns
  (:use [deuce.emacs-lisp :only (defun defvar) :as el])
  (:require [clojure.core :as c]
            [clojure.string :as s]
            [deuce.emacs.alloc :as alloc]
            [deuce.emacs.data :refer [car cdr setcar setcdr] :as data]
            [deuce.emacs.lread :as lread]
            [deuce.emacs-lisp.globals :as globals])
  (import [clojure.lang IPersistentCollection PersistentVector]
          [deuce.emacs.data CharTable]
          [deuce.emacs_lisp.cons Cons]
          [java.util List Map HashMap Collections Objects]
          [java.nio CharBuffer]
          [java.nio.charset Charset]
          [javax.xml.bind DatatypeConverter]
          [java.security MessageDigest])
  (:refer-clojure :exclude [concat assoc reverse nth identity require get sort]))

(defvar use-dialog-box nil
  "*Non-nil means mouse commands use dialog boxes to ask questions.
  This applies to `y-or-n-p' and `yes-or-no-p' questions asked by commands
  invoked by mouse clicks and mouse menu items.

  On some platforms, file selection dialogs are also enabled if this is
  non-nil.

  You can customize this variable.")

(defvar use-file-dialog nil
  "*Non-nil means mouse commands use a file dialog to ask for files.
  This applies to commands from menus and tool bar buttons even when
  they are initiated from the keyboard.  If `use-dialog-box' is nil,
  that disables the use of a file dialog, regardless of the value of
  this variable.

  You can customize this variable.")

(defvar features nil
  "A list of symbols which are the features of the executing Emacs.
  Used by `featurep' and `require', and altered by `provide'.")


(def ^:dynamic ^:private *symbol-plists* (atom {}))

(defn ^:private last-cons [l]
  (if (not (data/consp (cdr l))) l (recur (cdr l))))

(defun provide (feature &optional subfeatures)
  "Announce that FEATURE is a feature of the current Emacs.
  The optional argument SUBFEATURES should be a list of symbols listing
  particular subfeatures supported in this version of FEATURE."
  (when-not (some #{feature} globals/features)
    (el/setq features (cons feature globals/features)))
  feature)

(defun widget-get (widget property)
  "In WIDGET, get the value of PROPERTY.
  The value could either be specified when the widget was created, or
  later with `widget-put'."
  )

(defun string-as-multibyte (string)
  "Return a multibyte string with the same individual bytes as STRING.
  If STRING is multibyte, the result is STRING itself.
  Otherwise it is a newly created string, with no text properties.

  If STRING is unibyte and contains an individual 8-bit byte (i.e. not
  part of a correct utf-8 sequence), it is converted to the corresponding
  multibyte character of charset `eight-bit'.
  See also `string-to-multibyte'.

  Beware, this often doesn't really do what you think it does.
  It is similar to (decode-coding-string STRING 'utf-8-emacs).
  If you're not sure, whether to use `string-as-multibyte' or
  `string-to-multibyte', use `string-to-multibyte'."
  (let [utf-8 (.newEncoder (Charset/forName "UTF-8"))]
    (String. (.array (.encode utf-8 (CharBuffer/wrap string))) (.charset utf-8))))

(declare plist-put)

(defun lax-plist-put (plist prop val)
  "Change value in PLIST of PROP to VAL, comparing with `equal'.
  PLIST is a property list, which is a list of the form
  (PROP1 VALUE1 PROP2 VALUE2 ...).  PROP and VAL are any objects.
  If PROP is already a property on the list, its value is set to VAL,
  otherwise the new PROP VAL pair is added.  The new plist is returned;
  use `(setq x (lax-plist-put x prop val))' to be sure to use the new value.
  The PLIST is modified by side effects."
  (plist-put prop val))

(defun safe-length (list)
  "Return the length of a list, but avoid error or infinite loop.
  This function never gets an error.  If LIST is not really a list,
  it returns 0.  If LIST is circular, it returns a finite value
  which is at least the number of distinct elements."
  (if (instance? Cons list)
    (loop [pair list
           length 1]
      (if (and (instance? Cons (cdr pair)))
        (recur (cdr pair) (inc length))
        length))
    (if (coll? list)
      (count list)
      0)))

(declare equal mem)

(defun member (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
  The value is actually the tail of LIST whose car is ELT."
  (mem equal elt list))

(defun copy-hash-table (table)
  "Return a copy of hash table TABLE."
  (HashMap. table))

(defun append (&rest sequences)
  "Concatenate all the arguments and make the result a list.
  The result is a list whose elements are the elements of all the arguments.
  Each argument may be a list, vector or string.
  The last argument is not copied, just used as the tail of the new list."
  (let [sequences (remove empty? sequences)]
    (if (> (count sequences) 1)
      (let [l (apply alloc/list (apply c/concat (seq (butlast sequences))))]
        (setcdr (last-cons l) (last sequences))
        l)
      (first sequences))))

(defun mapconcat (function sequence separator)
  "Apply FUNCTION to each element of SEQUENCE, and concat the results as strings.
  In between each pair of results, stick in SEPARATOR.  Thus, \" \" as
  SEPARATOR results in spaces between the values returned by FUNCTION.
  SEQUENCE may be a list, a vector, a bool-vector, or a string."
  (s/join separator (map (el/fun function) (seq sequence))))

(defun compare-strings (str1 start1 end1 str2 start2 end2 &optional ignore-case)
  "Compare the contents of two strings, converting to multibyte if needed.
  In string STR1, skip the first START1 characters and stop at END1.
  In string STR2, skip the first START2 characters and stop at END2.
  END1 and END2 default to the full lengths of the respective strings.

  Case is significant in this comparison if IGNORE-CASE is nil.
  Unibyte strings are converted to multibyte for comparison.

  The value is t if the strings (or specified portions) match.
  If string STR1 is less, the value is a negative number N;
    - 1 - N is the number of characters that match at the beginning.
  If string STR1 is greater, the value is a positive number N;
    N - 1 is the number of characters that match at the beginning."
  (let [[ str1 str2] (if ignore-case
                       [(s/lower-case str1) (s/lower-case str2)]
                       [str1 str2])]
    (compare (subs str1 start1 end1) (subs str2 start2 end2))))

(defun copy-alist (alist)
  "Return a copy of ALIST.
  This is an alist which represents the same mapping from objects to objects,
  but does not share the alist structure with ALIST.
  The objects mapped (cars and cdrs of elements of the alist)
  are shared, however.
  Elements of ALIST that are not conses are also shared."
  alist)

(defun secure-hash (algorithm object &optional start end binary)
  "Return the secure hash of OBJECT, a buffer or string.
  ALGORITHM is a symbol specifying the hash to use:
  md5, sha1, sha224, sha256, sha384 or sha512.

  The two optional arguments START and END are positions specifying for
  which part of OBJECT to compute the hash.  If nil or omitted, uses the
  whole OBJECT.

  If BINARY is non-nil, returns a string in binary form."
  (let [hash (.digest (MessageDigest/getInstance (str algorithm))
                      (.getBytes (subs object (or start 0) (or end (count object))) "UTF-8"))]
    (if binary hash
        (apply str (map #(format "%02x" %) hash)))))

(defun copy-sequence (arg)
  "Return a copy of a list, vector, string or char-table.
  The elements of a list or vector are not copied; they are shared
  with the original."
  (condp some [arg]
    data/char-table-p (CharTable. (.defalt arg) (atom @(.parent arg)) (.purpose arg)
                                  (apply alloc/vector (.contents arg))
                                  (when (.extras arg)
                                    (apply alloc/vector (.extras arg))))
    data/listp (apply alloc/list arg)
    data/vectorp (apply alloc/vector arg)
    data/stringp (apply alloc/string arg)))

(defun string-as-unibyte (string)
  "Return a unibyte string with the same individual bytes as STRING.
  If STRING is unibyte, the result is STRING itself.
  Otherwise it is a newly created string, with no text properties.
  If STRING is multibyte and contains a character of charset
  `eight-bit', it is converted to the corresponding single byte."
  (let [ascii (.newEncoder (Charset/forName "US-ASCII"))]
    (String. (.array (.encode ascii (CharBuffer/wrap string))) (.charset ascii))))

(defun sxhash (obj)
  "Compute a hash code for OBJ and return it as integer."
  (hash obj))

(defun md5 (object &optional start end coding-system noerror)
  "Return MD5 message digest of OBJECT, a buffer or string.

  A message digest is a cryptographic checksum of a document, and the
  algorithm to calculate it is defined in RFC 1321.

  The two optional arguments START and END are character positions
  specifying for which part of OBJECT the message digest should be
  computed.  If nil or omitted, the digest is computed for the whole
  OBJECT.

  The MD5 message digest is computed from the result of encoding the
  text in a coding system, not directly from the internal Emacs form of
  the text.  The optional fourth argument CODING-SYSTEM specifies which
  coding system to encode the text with.  It should be the same coding
  system that you used or will use when actually writing the text into a
  file.

  If CODING-SYSTEM is nil or omitted, the default depends on OBJECT.  If
  OBJECT is a buffer, the default for CODING-SYSTEM is whatever coding
  system would be chosen by default for writing this text into a file.

  If OBJECT is a string, the most preferred coding system (see the
  command `prefer-coding-system') is used.

  If NOERROR is non-nil, silently assume the `raw-text' coding if the
  guesswork fails.  Normally, an error is signaled in such case."
  (secure-hash 'md5 object start end))

(defun widget-apply (widget property &rest args)
  "Apply the value of WIDGET's PROPERTY to the widget itself.
  ARGS are passed as extra arguments to the function."
  )

(defun hash-table-p (obj)
  "Return t if OBJ is a Lisp hash table object."
  (instance? Map obj))

(declare del)

(defun delete (elt seq)
  "Delete by side effect any occurrences of ELT as a member of SEQ.
  SEQ must be a list, a vector, or a string.
  The modified SEQ is returned.  Comparison is done with `equal'.
  If SEQ is not a list, or the first member of SEQ is ELT, deleting it
  is not a side effect; it is simply using a different sequence.
  Therefore, write `(setq foo (delete element foo))'
  to be sure of changing the value of `foo'."
  (condp instance? seq
    Cons (del data/= elt seq)
    PersistentVector (filterv (partial data/= elt) seq)
    String (clojure.string/replace seq elt "")))

(defun locale-info (item)
  "Access locale data ITEM for the current C locale, if available.
  ITEM should be one of the following:

  `codeset', returning the character set as a string (locale item CODESET);

  `days', returning a 7-element vector of day names (locale items DAY_n);

  `months', returning a 12-element vector of month names (locale items MON_n);

  `paper', returning a list (WIDTH HEIGHT) for the default paper size,
    both measured in millimeters (locale items PAPER_WIDTH, PAPER_HEIGHT).

  If the system can't provide such information through a call to
  `nl_langinfo', or if ITEM isn't from the list above, return nil.

  See also Info node `(libc)Locales'.

  The data read from the system are decoded using `locale-coding-system'."
  )

(defun string-to-multibyte (string)
  "Return a multibyte string with the same individual chars as STRING.
  If STRING is multibyte, the result is STRING itself.
  Otherwise it is a newly created string, with no text properties.

  If STRING is unibyte and contains an 8-bit byte, it is converted to
  the corresponding multibyte character of charset `eight-bit'.

  This differs from `string-as-multibyte' by converting each byte of a correct
  utf-8 sequence to an eight-bit character, not just bytes that don't form a
  correct sequence."
  (string-as-multibyte string))

(defun eql (obj1 obj2)
  "Return t if the two args are the same Lisp object.
  Floating-point numbers of equal value are `eql', but they may not be `eq'."
  (cond
    (and (float? obj1) (float obj2)) (== obj1 obj2)
    :else (data/eq obj1 obj2)))

(defn ^:private plist-map [plist]
  (if (instance? Map plist) plist (into {} (map vec (partition 2 plist)))))

(defun plist-get (plist prop)
  "Extract a value from a property list.
  PLIST is a property list, which is a list of the form
  (PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
  corresponding to the given PROP, or nil if PROP is not one of the
  properties on the list.  This function never signals an error."
  ((plist-map plist) prop))

(defun elt (sequence n)
  "Return element of SEQUENCE at index N."
  (c/nth n sequence))

(defun base64-encode-string (string &optional no-line-break)
  "Base64-encode STRING and return the result.
  Optional second argument NO-LINE-BREAK means do not break long lines
  into shorter lines."
  (DatatypeConverter/printBase64Binary (.getBytes string "UTF-8")))

(defun equal-including-properties (o1 o2)
  "Return t if two Lisp objects have similar structure and contents.
  This is like `equal' except that it compares the text properties
  of strings.  (`equal' ignores text properties.)"
  (c/= o1 o2))

(defun substring-no-properties (string &optional from to)
  "Return a substring of STRING, without text properties.
  It starts at index FROM and ends before TO.
  TO may be nil or omitted; then the substring runs to the end of STRING.
  If FROM is nil or omitted, the substring starts at the beginning of STRING.
  If FROM or TO is negative, it counts from the end.

  With one argument, just copy STRING without its properties."
  (subs string (or from 0) (or to (count string))))

(defun random (&optional limit)
  "Return a pseudo-random number.
  All integers representable in Lisp are equally likely.
    On most systems, this is 29 bits' worth.
  With positive integer LIMIT, return random number in interval [0,LIMIT).
  With argument t, set the random number seed from the current time and pid.
  Other values of LIMIT are ignored."
  (rand-int limit))

(defun concat (&rest sequences)
  "Concatenate all the arguments and make the result a string.
  The result is a string whose elements are the elements of all the arguments.
  Each argument may be a string or a list or vector of characters (integers)."
  (apply str (map char (apply c/concat sequences))))

(defun string-bytes (string)
  "Return the number of bytes in STRING.
  If STRING is multibyte, this may be greater than the length of STRING."
  (count (.getBytes string)))

(defun assoc (key list)
  "Return non-nil if KEY is `equal' to the car of an element of LIST.
  The value is actually the first element of LIST whose car equals KEY."
  (some #(c/and (instance? Cons %) (equal key (.car %)) %) (seq list)))

(defun remhash (key table)
  "Remove KEY from TABLE."
  (.remove table key)
  nil)

(defun yes-or-no-p (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes.
  PROMPT is the string to display to ask the question.  It should end in
  a space; `yes-or-no-p' adds \"(yes or no) \" to it.

  The user must confirm the answer with RET, and can edit it until it
  has been confirmed.

  Under a windowing system a dialog box will be used if `last-nonmenu-event'
  is nil, and `use-dialog-box' is non-nil."
  )

(defun hash-table-count (table)
  "Return the number of elements in TABLE."
  (count table))

(defun clear-string (string)
  "Clear the contents of STRING.
  This makes STRING unibyte and may change its length."
  )

(defn ^:private del [f elt list]
  (loop [prev list
         curr (cdr list)]
    (when (car curr)
      (when (f elt (car curr))
        (setcar prev (cdr curr)))
      (recur (cdr prev)
             (cdr curr))))
  (if (data/eq elt (car list))
    (cdr list) list))

(defn ^:private mem [f elt list]
  (loop [list list]
    (if (f elt (car list))
      list
      (when-let [list (cdr list)]
        (recur list)))))

(defun delq (elt list)
  "Delete by side effect any occurrences of ELT as a member of LIST.
  The modified LIST is returned.  Comparison is done with `eq'.
  If the first member of LIST is ELT, there is no way to remove it by side effect;
  therefore, write `(setq foo (delq element foo))'
  to be sure of changing the value of `foo'."
  (del data/eq elt list))

(defun assq (key list)
  "Return non-nil if KEY is `eq' to the car of an element of LIST.
  The value is actually the first element of LIST whose car is KEY.
  Elements of LIST that are not conses are ignored."
  (first (filter #(data/eq key (data/car-safe %)) (seq list))))

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
  (string-to-multibyte string))

(defun string-equal (s1 s2)
  "Return t if two strings have identical contents.
  Case is significant, but text properties are ignored.
  Symbols are also allowed; their print names are used instead."
  (= (str s1) (str s2)))

(defun mapcar (function sequence)
  "Apply FUNCTION to each element of SEQUENCE, and make a list of the results.
  The result is a list just as long as SEQUENCE.
  SEQUENCE may be a list, a vector, a bool-vector, or a string."
  (apply alloc/list (map (el/fun function) (seq sequence))))

(defun fillarray (array item)
  "Store each element of ARRAY with ITEM.
  ARRAY is a vector, string, char-table, or bool-vector."
  (dotimes [n (count array)]
    (aset array n item))
  array)

(defun load-average (&optional use-floats)
  "Return list of 1 minute, 5 minute and 15 minute load averages.

  Each of the three load averages is multiplied by 100, then converted
  to integer.

  When USE-FLOATS is non-nil, floats will be used instead of integers.
  These floats are not multiplied by 100.

  If the 5-minute or 15-minute load averages are not available, return a
  shortened list, containing only those averages which are available.

  An error is thrown if the load average can't be obtained.  In some
  cases making it work would require Emacs being installed setuid or
  setgid so that it can read kernel information, and that usually isn't
  advisable."
  )

(defun base64-encode-region (beg end &optional no-line-break)
  "Base64-encode the region between BEG and END.
  Return the length of the encoded text.
  Optional third argument NO-LINE-BREAK means do not break long lines
  into shorter lines."
  )

(defun hash-table-weakness (table)
  "Return the weakness of TABLE."
  nil)

(defun clrhash (table)
  "Clear hash table TABLE and return it."
  (empty table))

(defun vconcat (&rest sequences)
  "Concatenate all the arguments and make the result a vector.
  The result is a vector whose elements are the elements of all the arguments.
  Each argument may be a list, vector or string."
  (alloc/vector (apply c/concat sequences)))

(defun make-hash-table (&rest keyword-args)
  "Create and return a new hash table.

  Arguments are specified as keyword/argument pairs.  The following
  arguments are defined:

  :test TEST -- TEST must be a symbol that specifies how to compare
  keys.  Default is `eql'.  Predefined are the tests `eq', `eql', and
  `equal'.  User-supplied test and hash functions can be specified via
  `define-hash-table-test'.

  :size SIZE -- A hint as to how many elements will be put in the table.
  Default is 65.

  :rehash-size REHASH-SIZE - Indicates how to expand the table when it
  fills up.  If REHASH-SIZE is an integer, increase the size by that
  amount.  If it is a float, it must be > 1.0, and the new size is the
  old size multiplied by that factor.  Default is 1.5.

  :rehash-threshold THRESHOLD -- THRESHOLD must a float > 0, and <= 1.0.
  Resize the hash table when the ratio (number of entries / table size)
  is greater than or equal to THRESHOLD.  Default is 0.8.

  :weakness WEAK -- WEAK must be one of nil, t, `key', `value',
  `key-or-value', or `key-and-value'.  If WEAK is not nil, the table
  returned is a weak table.  Key/value pairs are removed from a weak
  hash table when there are no non-weak references pointing to their
  key, value, one of key or value, or both key and value, depending on
  WEAK.  WEAK t is equivalent to `key-and-value'.  Default value of WEAK
  is nil."
  (let [{:keys [size rehash-threshold] :or {size 65 rehash-threshold 0.8}} (apply hash-map keyword-args)]
    (HashMap. size rehash-threshold)))

(defun rassoc (key list)
  "Return non-nil if KEY is `equal' to the cdr of an element of LIST.
  The value is actually the first element of LIST whose cdr equals KEY."
  (some #(c/and (instance? Cons %) (equal key (cdr %)) %) (seq list)))

(defun equal (o1 o2)
  "Return t if two Lisp objects have similar structure and contents.
  They must have the same data type.
  Conses are compared by comparing the cars and the cdrs.
  Vectors and strings are compared element by element.
  Numbers are compared by value, but integers cannot equal floats.
   (Use `=' if you want integers and floats to be able to be equal.)
  Symbols must match exactly."
  (Objects/deepEquals o1 o2))

(declare reverse)

(defun nreverse (list)
  "Reverse LIST by modifying cdr pointers.
  Return the reversed list."
  (loop [l list
         n (cdr list)
         r nil]
    (if (data/consp n)
      (do (setcdr l r)
          (recur n (cdr n) l))
      r)))

(defun reverse (list)
  "Reverse LIST, copying.  Return the reversed list.
  See also the function `nreverse', which is used more often."
  (when (seq list)
    (apply alloc/list (c/reverse (seq list)))))

(defun nthcdr (n list)
  "Take cdr N times on LIST, return the result."
  (loop [n n list list]
    (if (pos? n)
      (recur (dec n) (cdr list))
      list)))

(defun hash-table-rehash-size (table)
  "Return the current rehash size of TABLE."
  )

(defun put (symbol propname value)
  "Store SYMBOL's PROPNAME property with value VALUE.
  It can be retrieved with `(get SYMBOL PROPNAME)'."
  (swap! *symbol-plists* assoc-in [symbol propname] value)
  value)

(defun base64-decode-region (beg end)
  "Base64-decode the region between BEG and END.
  Return the length of the decoded text.
  If the region can't be decoded, signal an error and don't modify the buffer."
  )

(defun nth (n list)
  "Return the Nth element of LIST.
  N counts from zero.  If LIST is not that long, nil is returned."
  (c/nth (seq list) n nil))

(defun string-to-unibyte (string)
  "Return a unibyte string with the same individual chars as STRING.
  If STRING is unibyte, the result is STRING itself.
  Otherwise it is a newly created string, with no text properties,
  where each `eight-bit' character is converted to the corresponding byte.
  If STRING contains a non-ASCII, non-`eight-bit' character,
  an error is signaled."
  (string-as-unibyte string))

(defun nconc (&rest lists)
  "Concatenate any number of lists by altering them.
  Only the last argument is not altered, and need not be a list."
  (let [lists (remove empty? lists)]
    (when (> (count lists) 1)
      (loop [ls (rest lists)
             last (last-cons (first lists))]
        (setcdr last (first ls))
        (when (seq (rest ls))
          (recur (rest ls)
                 (last-cons (first ls))))))
    (first lists)))

(defun length (sequence)
  "Return the length of vector, list or string SEQUENCE.
  A byte-code function object is also allowed.
  If the string contains multibyte characters, this is not necessarily
  the number of bytes in the string; it is the number of characters.
  To get the number of bytes, use `string-bytes'."
  (condp instance? sequence
    Cons (loop [cons sequence
                length 0]
           (if (data/consp cons)
             (recur (cdr cons) (inc length))
             (do
               (el/check-type 'listp cons)
               length)))
    CharTable (count (.contents sequence))
    (count sequence)))

(defun memq (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.
  The value is actually the tail of LIST whose car is ELT."
  (mem data/eq elt list))

(defun memql (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `eql'.
  The value is actually the tail of LIST whose car is ELT."
  (mem eql elt list))

(defun gethash (key table &optional dflt)
  "Look up KEY in TABLE and return its associated value.
  If KEY is not found, return DFLT which defaults to nil."
  (c/get table key dflt))

(defun identity (arg)
  "Return the argument unchanged."
  arg)

(defun define-hash-table-test (name test hash)
  "Define a new hash table test with name NAME, a symbol.

  In hash tables created with NAME specified as test, use TEST to
  compare keys, and HASH for computing hash codes of keys.

  TEST must be a function taking two arguments and returning non-nil if
  both arguments are the same.  HASH must be a function taking one
  argument and return an integer that is the hash code of the argument.
  Hash code computation should use the whole value range of integers,
  including negative integers."
  )

(defun hash-table-size (table)
  "Return the size of TABLE.
  The size can be used as an argument to `make-hash-table' to create
  a hash table than can hold as many elements as TABLE holds
  without need for resizing."
  (count table))

(declare featurep)

(defun require (feature &optional filename noerror)
  "If feature FEATURE is not loaded, load it from FILENAME.
  If FEATURE is not a member of the list `features', then the feature
  is not loaded; so load the file FILENAME.
  If FILENAME is omitted, the printname of FEATURE is used as the file name,
  and `load' will try to load this name appended with the suffix `.elc' or
  `.el', in that order.  The name without appended suffix will not be used.
  See `get-load-suffixes' for the complete list of suffixes.
  If the optional third argument NOERROR is non-nil,
  then return nil if the file is not found instead of signaling an error.
  Normally the return value is FEATURE.
  The normal messages at start and end of loading FILENAME are suppressed."
  (when-not (featurep feature)
    (lread/load (or filename (name feature)) noerror true))
  feature)

(defun get (symbol propname)
  "Return the value of SYMBOL's PROPNAME property.
  This is the last value stored with `(put SYMBOL PROPNAME VALUE)'."
  (get-in @*symbol-plists* [symbol propname]))

(defun lax-plist-get (plist prop)
  "Extract a value from a property list, comparing with `equal'.
  PLIST is a property list, which is a list of the form
  (PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
  corresponding to the given PROP, or nil if PROP is not
  one of the properties on the list."
  (plist-get plist prop))

(defun maphash (function table)
  "Call FUNCTION for all entries in hash table TABLE.
  FUNCTION is called with two arguments, KEY and VALUE."
  (map (el/fun function) table)
  nil)

(defun rassq (key list)
  "Return non-nil if KEY is `eq' to the cdr of an element of LIST.
  The value is actually the first element of LIST whose cdr is KEY."
  (first (filter #(data/eq key (data/cdr-safe %))  list)))

(defun string-make-unibyte (string)
  "Return the unibyte equivalent of STRING.
  Multibyte character codes are converted to unibyte according to
  `nonascii-translation-table' or, if that is nil, `nonascii-insert-offset'.
  If the lookup in the translation table fails, this function takes just
  the low 8 bits of each character."
  (string-to-unibyte string))

(defun string-lessp (s1 s2)
  "Return t if first arg string is less than second in lexicographic order.
  Case is significant.
  Symbols are also allowed; their print names are used instead."
  (neg? (compare (str s1) (str s2))))

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
  to the end of STRING.

  The STRING argument may also be a vector.  In that case, the return
  value is a new vector that contains the elements between index FROM
  (inclusive) and index TO (exclusive) of that vector argument."
  (let [idx #(if (neg? %) (+ % (count string)) %)]
    (subs string (idx from) (idx (or to (count string))))))

(defun featurep (feature &optional subfeature)
  "Return t if FEATURE is present in this Emacs.

  Use this to conditionalize execution of lisp code based on the
  presence or absence of Emacs or environment extensions.
  Use `provide' to declare that a feature is available.  This function
  looks at the value of the variable `features'.  The optional argument
  SUBFEATURE can be used to check a specific subfeature of FEATURE."
  (boolean (some #{feature} globals/features)))

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
  (if-let [rest (memq prop plist)]
    (do (setcar (cdr rest) val)
        plist)
    (nconc plist (alloc/list prop val))))

(defun puthash (key value table)
  "Associate KEY with VALUE in hash table TABLE.
  If KEY is already present in table, replace its current value with
  VALUE.  In any case, return VALUE."
  (.put table key value)
  value)

(defun hash-table-test (table)
  "Return the test TABLE uses."
  )

(defun mapc (function sequence)
  "Apply FUNCTION to each element of SEQUENCE for side effects only.
  Unlike `mapcar', don't accumulate the results.  Return SEQUENCE.
  SEQUENCE may be a list, a vector, a bool-vector, or a string."
  (dorun (map (el/fun function) sequence))
  sequence)

(defun plist-member (plist prop)
  "Return non-nil if PLIST has the property PROP.
  PLIST is a property list, which is a list of the form
  (PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol.
  Unlike `plist-get', this allows you to distinguish between a missing
  property and a property with the value nil.
  The value is actually the tail of PLIST whose car is PROP."
  ((plist-map plist) prop))

(defun sort (list predicate)
  "Sort LIST, stably, comparing elements using PREDICATE.
  Returns the sorted list.  LIST is modified by side effects.
  PREDICATE is called with two elements of LIST, and should return non-nil
  if the first element should sort before the second."
  (c/sort (fn [x y] (if (predicate x y) -1 1)) list))

(defun base64-decode-string (string)
  "Base64-decode STRING and return the result."
  (String. (DatatypeConverter/parseBase64Binary string) "UTF-8"))

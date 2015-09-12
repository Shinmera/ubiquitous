#|
 This file is a part of ubiquitous
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.ubiquitous)

(defmacro setdocs (&body pairs)
  `(progn
     ,@(loop for (var doc) in pairs
             collect (destructuring-bind (var &optional (type 'function))
                         (if (listp var) var (list var))
                       `(setf (documentation ',var ',type) ,doc)))))

;; accessor.lisp
(setdocs
  ((*nothing* variable)
   "Variable with a value used to represent an inexistent value.")

  (field
   "Access FIELD on OBJECT if possible. Returns DEFAULT if FIELD is not present.
The secondary return value is a boolean depicting whether the field could be found.

This is SETF-able. However, while some objects and field combinations may be used
to read a field, an equivalent SETF method must not necessarily exist.

Note that if there is no matching method to look up the requested field, an error
is signalled.")

  (remfield
   "Removes FIELD from OBJECT if possible.
The secondary return value is a boolean depicting whether the field was removed.

Note that if there is no matching method to look up the requested field, an error
is signalled.")

  (augment
   "Attempts to augment OBJECT on FIELD to be able to host a SECONDARY place.

This is done by (SETF FIELD) a hash-table on the given OBJECT and FIELD. The
type of SECONDARY decides the hash-table test to use:
  SYMBOL, INTEGER, CHARACTER          --- EQL
  STRING, BIT-VECTOR, PATHNAME        --- EQUAL
  ARRAY, STRUCTURE-OBJECT, HASH-TABLE --- EQUALP

See FIELD"))

;; config.lisp
(setdocs
  ((*commit* variable)
   "When non-NIL, an OFFLOAD is performed after a call to (SETF VALUE) or REMVALUE.")
  
  (value
   "Traverses *STORAGE* by the fields in PATH and returns the value if it can be found.
The secondary return value is a boolean depicting whether the field could be found.

This is SETF-able. If a PATH is set that is made up of fields that do not exist yet,
then these fields are automatically created as necessary (if possible) by usage of
AUGMENT. Setting with no PATH given sets the value of *STORAGE*. After setting a value,
OFFLOAD is called, unless *COMMIT* is NIL

See FIELD
See AUGMENT
See OFFLOAD
See *COMMIT*")

  (remvalue
   "Removes the value denoted by the PATH.
The secondary return value is a boolean depicting whether the field could be found.

First traverses *STORAGE* until the last field in PATH by FIELD, then uses REMFIELD
on the last remaining field. If no PATH is given, the *STORAGE* is reset to an empty
hash-table.

See FIELD
See REMFIELD")

  (defaulted-value
   "Same as VALUE, but automatically returns and sets DEFAULT if the field cannot be found.

See VALUE")

  (call-with-transaction
   "Calls FUNCTION with *COMMIT* set to NIL and calls OFFLOAD after the function exits.

The keyword parameters replace the bindings for *STORAGE* *STORAGE-TYPE* and
*STORAGE-PATHNAME* respectively.

See *COMMIT*
See OFFLOAD")

  ((with-transaction)
   "Executes BODY within a transaction.

See CALL-WITH-TRANSACTION"))

;; pathname.lisp
(setdocs
  (config-directory
   "Returns a hopefully suitable directory for ubiquitous configuration files.

On Windows this is (USER-HOMEDIR-PATHNAME)/AppData/Local/common-lisp/ubiquitous
On other systems   (USER-HOMEDIR-PATHNAME)/.config/common-lisp/ubiquitous")
  
  (config-pathname
   "Returns a pathname with the proper directory and type set.

See CONFIG-DIRECTORY")

  (designator-pathname
   "Attempts to automatically find the proper pathname for the given DESIGNATOR and TYPE.

If DESIGNATOR is..
  An absolute PATHNAME:
    The pathname is used as-is.

  A relative PATHNAME: 
    The pathname is merged with that of CONFIG-DIRECTORY.

  A STRING:
    The string is turned into a pathname by PATHNAME and merged with CONFIG-PATHNAME.

  An uninterned or keyword SYMBOL:
    The symbol-name is used as the pathname-name and merged with CONFIG-PATHNAME.

  A SYMBOL from the CL package:
    An error is raised, as in almost all certainty don't want to do this. Using a CL
    symbol would very likely run you into configuration conflicts with other applications.

  An interned SYMBOL:
    A pathname with the symbol's package-name as relative directory and the symbol-name
    as pathname-name is merged with CONFIG-PATHNAME.

Examples:
(designator-pathname #p\"/a\" :lisp)   --- #p\"/a\"
(designator-pathname #p\"a\" :lisp)    --- #p\"~/.config/common-lisp/ubiquitous/a\"
(designator-pathname \"a\" :lisp)      --- #p\"~/.config/common-lisp/ubiquitous/a.conf.lisp\"
(designator-pathname :foo :lisp)     --- #p\"~/.config/common-lisp/ubiquitous/foo.conf.lisp\"
(designator-pathname #:foo :lisp)    --- #p\"~/.config/common-lisp/ubiquitous/foo.conf.lisp\"
(designator-pathname 'cl:find :lisp) --- ERROR
(designator-pathname 'foo:bar :lisp) --- #p\"~/.config/common-lisp/ubiquitous/foo/bar.conf.lisp\""))

;; storage.lisp
(setdocs
  ((*storage* variable)
   "Special variable containing the current root storage object.
Defaults to an EQUAL hash-table.")

  ((*storage-type* variable)
   "An indicator for the type of storage format to use.
Defaults to :LISP

Only used as a discerning argument to READ/WRITE-STORAGE.")

  ((*storage-pathname* variable)
   "The pathname for the file where the current *STORAGE* is stored.
Defaults to (DESIGNATOR-PATHNAME :GLOBAL *STORAGE-TYPE*).

See DESIGNATOR-PATHNAME
See *STORAGE-TYPE*")

  (read-storage
   "Reads a storage object from STREAM, which must be stored in a format suitable for TYPE.
Returns the read storage object.")

  (write-storage
   "Writes the STORAGE object to STREAM in a format suitable for TYPE.
Returns the written STORAGE object.")

  ((with-storage)
   "Binds *STORAGE* to the given STORAGE object, ensuring a local configuration.")

  (restore
   "Restores *STORAGE* by reading it from file if possible and returns it.

The file used to read the storage is calculated by passing
DESIGNATOR (defaulting to *STORAGE-PATHNAME*) and TYPE (defaulting to
*STORAGE-TYPE*) to DESIGNATOR-PATHNAME. If it exists, a stream is opened
and subsequently passed to READ-STORAGE. The result thereof is used as
the new storage object.

This sets *STORAGE*, *STORAGE-TYPE*, and *STORAGE-PATHNAME*

See *STORAGE*
See *STORAGE-TYPE*
See *STORAGE-PATHNAME*
See DESIGNATOR-PATHNAME
See READ-STORAGE")

  (offload
   "Offloads *STORAGE* by writing it to file.

The file used to read the storage is calculated by passing
DESIGNATOR (defaulting to *STORAGE-PATHNAME*) and TYPE (defaulting to
*STORAGE-TYPE*) to DESIGNATOR-PATHNAME.

The file is first written to a temporary one and then renamed to the
actual file to avoid potential errors or interruptions that would result
 in a garbled configuration file.

This sets *STORAGE-TYPE*, and *STORAGE-PATHNAME*

See *STORAGE*
See *STORAGE-TYPE*
See *STORAGE-PATHNAME*
See DESIGNATOR-PATHNAME
See WRITE-STORAGE")

  ((*ubiquitous-print-table* variable)
   "The pprint-dispatch-table used to write storage objects to file.")

  ((*ubiquitous-read-table* variable)
   "The readtable used to read storage objects from file.")

  ((*ubiquitous-readers* variable)
   "A hash table of functions invoked upon reading a special construct.")

  ((*ubiquitous-char* variable)
   "A string of two characters denoting the start and end of a special construct.")

  (ubiquitous-reader
   "Reader function invoked when encountering the first character of *UBIQUITOUS-CHAR*.")

  (ubiquitous-writer
   "Function to pretty print a generalised FORM to STREAM.
Emits a logical block wherein the FORM is printed.
The form must be a list and is stepped one by one. If the item in the list
is not a list, it is written readably to the stream. If it is a list, it is
written to the stream after a PPRINT-NEWLINE by PPRINT-LINEAR.")

  ((define-ubiquitous-writer)
   "Define a new function that produces a list of objects to be written to reproduce OBJECT of TYPE.")

  ((define-ubiquitous-reader)
   "Define a new function that produces an object of TYPE by parsing the read FORM."))

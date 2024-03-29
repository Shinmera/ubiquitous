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

In the case where the object is a function, the function is called as follows:
 (field func field default)      => (funcall func :get field default)
 (setf (field func field) value) => (funcall func :set field value) 

Note that if there is no matching method to look up the requested field, an error
is signalled.")

  (remfield
   "Removes FIELD from OBJECT if possible.
The secondary return value is a boolean depicting whether the field was removed.

In the case where the object is a function, the function is called as follows:
  (remfield func field)          => (funcall func :remove field)

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

  ((*changed* variable)
   "When non-NIL it means a change has occurred and the config should be offloaded.")
  
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
   "Calls FUNCTION with *COMMIT* set to NIL and offloads if necessary upon exit.

OFFLOAD is only called if *CHANGED* is non-NIL. Otherwise no change is assumed to
have taken place and the offload is prevented to avoid unnecessary writing.

The keyword parameters replace the bindings for *STORAGE* *STORAGE-TYPE* and
*STORAGE-PATHNAME* respectively.

See *COMMIT*
See *CHANGED*
See OFFLOAD")

  ((with-transaction)
   "Executes BODY within a transaction.

See CALL-WITH-TRANSACTION"))

;; metadata.lisp
(setdocs
  ((*metadata-version* variable)
   "The current version of the configuration metadata.

Should be a float.")
  
  ((*metadata-prefix* variable)
   "The prefix that is used to recognise the metadata header.")

  ((metadata-condition type)
   "Superclass for conditions related to configuration metadata.")

  ((unknown-version type)
   "Warning signalled when the metadata header contains an unknown version number.

See VERSION
See METADATA-CONDITION")

  (version
   "Returns the version captured by the condition.

See UNKNOWN-VERSION")

  ((bad-configuration-package type)
   "Error signalled when the metadata header refers to an unknown package.

See BAD-PACKAGE-NAME
See METADATA-CONDITION")

  (bad-package-name
   "Returns the bad package name that the condition captured.

See BAD-CONFIGURATION-PACKAGE")

  ((bad-metadata-header type)
   "Error signalled when the metadata header is malformed.

See HEADER
See METADATA-CONDITION")

  (header
   "Returns the header that is malformed.

See BAD-METADATA-HEADER")

  (find-metadata-package
   "Attempt to find a package of the given name for metadata package resolution.

This is like FIND-PACKAGE, except that it signals an error of type
BAD-CONFIGURATION-PACKAGE if no package could be found, and establishes
the USE-VALUE and CONTINUE restarts to recover from the error.

USE-VALUE lets you pass a package designator to use in place. Note that
if you do not pass a PACKAGE object, it will re-test the designator the
same way.

CONTINUE will instead use the current *PACKAGE*. 

Note that using an alternate package may lead to symbols with the wrong
home package in the processed configuration.

See BAD-CONFIGURATION-PACKAGE")

  (check-metadata
   "Checks whether the read metadata structure is valid.

Namely an error of type BAD-METADATA-HEADER is signalled if the metadata
is not a list, and is not a proper plist.

See BAD-METADATA-HEADER")

  (process-metadata
   "Processes the metadata and applies its effects.

This may signal conditions if the metadata is malformed, or the current
environment is not agreeable. Two restarts are established to recover:

USE-VALUE allows you to supply alternate metadata. The metadata is then
processed in place of the original.

CONTINUE will simply ignore the metadata and return successfully.

See CHECK-METADATA
See BAD-METADATA-HEADER
See BAD-CONFIGURATION-PACKAGE
See UNKNOWN-VERSION")

  (generate-metadata
   "Generates valid metadata for the current environment.")

  (prefix-p
   "Returns T if the PREFIX is a prefix of STRING.")

  (maybe-read-metadata
   "Attempts to read a metadata line from the stream.

If the data at the current stream starts with *METADATA-PREFIX*, the line is
consumed and read. The read metadata structure is returned. If reading fails,
an error of type BAD-METADATA-HEADER is signalled.

See *METADATA-PREFIX*
See BAD-METADATA-HEADER")

  (print-metadata
   "Writes a metadata header to the stream.

Writes a valid metadata header line to the stream, with the METADATA as the
header contents. The header will start with *METADATA-PREFIX* and will only
consume a single line.

See *METADATA-PREFIX*
See GENERATE-METADATA")

  ((with-processed-metadata)
   "Wraps the body in an environment where metadata can be safely applied and processes the given metadata within.

At the moment, this simply binds *PACKAGE* to ensure the package indicated
by the metadata can be applied without influencing the surrounding environment.

See PROCESS-METADATA"))

;; pathname.lisp
(setdocs
  (config-directory
   "Returns a hopefully suitable directory for ubiquitous configuration files.

On Windows this is (USER-HOMEDIR-PATHNAME)/AppData/Local/common-lisp/ubiquitous
On other systems   (USER-HOMEDIR-PATHNAME)/.config/common-lisp/ubiquitous")
  
  (config-pathname
   "Returns a pathname with the proper directory and type set.

See CONFIG-DIRECTORY")

  (package-directory
   "Returns the directory for config files suitable for this package.

By default:
For the CL package, an error is signalled.
For the KEYWORD and NIL packages, the CONFIG-DIRECTORY is returned.
Otherwise, a subdirectory within the CONFIG-DIRECTORY is returned according to the
package's name.

The user may add methods to this function to customise the behaviour of their own
packages.

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

  A SYMBOL:
    A pathname with the symbol's symbol-name as pathname-name, the CONFIG-PATHNAME's
    pathname-type, and the defaults from PACKAGE-PATHNAME is constructed.

Examples:
(designator-pathname #p\"/a\" :lisp)   --- #p\"/a\"
(designator-pathname #p\"a\" :lisp)    --- #p\"~/.config/common-lisp/ubiquitous/a\"
(designator-pathname \"a\" :lisp)      --- #p\"~/.config/common-lisp/ubiquitous/a.conf.lisp\"
(designator-pathname :foo :lisp)     --- #p\"~/.config/common-lisp/ubiquitous/foo.conf.lisp\"
(designator-pathname #:foo :lisp)    --- #p\"~/.config/common-lisp/ubiquitous/foo.conf.lisp\"
(designator-pathname 'cl:find :lisp) --- ERROR
(designator-pathname 'foo:bar :lisp) --- #p\"~/.config/common-lisp/ubiquitous/foo/bar.conf.lisp\"

See PACKAGE-PATHNAME
See CONFIG-DIRECTORY"))

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

  (lazy-loader
   "A function that is to be used as a direct *STORAGE* value to delay the restoring.

When called, the function will call RESTORE and then delegate the given
action to the proper function (FIELD, (SETF FIELD), REMFIELD) using the
*STORAGE* as object.

See *STORAGE*
See FIELD
See REMFIELD
See WITH-LOCAL-STORAGE")

  ((with-local-storage)
   "Useful for completely encapsulating the storage in a local block.

Unlike WITH-STORAGE, this also binds the *STORAGE-TYPE* and
*STORAGE-PATHNAME*. If TRANSACTION is non-NIL, WITH-TRANSACTION is
used, and otherwise a simple LET*. STORAGE defaults to the LAZY-LOADER
function, meaning that if the storage is never accessed, it is never
loaded to begin with. This, along with WITH-TRANSACTION can be a
good optimisation to avoid unnecessary disk access.

See *STORAGE*
See *STORAGE-TYPE*
See *STORAGE-PATHNAME*
See WITH-TRANSACTION
See LAZY-LOADER")
  
  ((no-storage-file type)
   "Condition signalled when the storage FILE to be RESTOREd does not exist.

This does not usually denote a problem, but can be useful to know about
if you want perform certain actions on what is probably a first-time
launch.

See FILE
See RESTORE")

  (file
   "To be used on NO-STORAGE-FILE, returns the pathname to the file that could not be found.")

  (restore
   "Restores *STORAGE* by reading it from file if possible and returns it.

The file used to read the storage is calculated by passing
DESIGNATOR (defaulting to *STORAGE-PATHNAME*) and TYPE (defaulting to
*STORAGE-TYPE*) to DESIGNATOR-PATHNAME. If it exists, a stream is opened
and subsequently passed to READ-STORAGE. The result thereof is used as
the new storage object. If it does not exist, a warning of type 
NO-STORAGE-FILE is signalled and a new EQUAL hash-table is used for the
storage object (unless a restart is invoked of course).

This sets *STORAGE*, *STORAGE-TYPE*, *STORAGE-PATHNAME*, and *CHANGED*.

During OFFLOAD, the following restarts are active:
  USE-NEW-STORAGE  Takes one argument to use as the new storage instead.
  ABORT            Aborts and does not set any of the usual variables.

See *STORAGE*
See *STORAGE-TYPE*
See *STORAGE-PATHNAME*
See *CHANGED*
See NO-STORAGE-FILE
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

This sets *STORAGE-TYPE*, *STORAGE-PATHNAME*, and *CHANGED*.

During OFFLOAD, the following restarts are active:
   ABORT  Aborts and does not set any of the usual variables.

See *STORAGE*
See *STORAGE-TYPE*
See *STORAGE-PATHNAME*
See *CHANGED*
See DESIGNATOR-PATHNAME
See WRITE-STORAGE")

  (destroy
   "Destroys *STORAGE* by deleting its file and restoring it to an empty hash table.

The file used to destroy the storage is calculated by passing
DESIGNATOR (defaulting to *STORAGE-PATHNAME*) and TYPE (defaulting to
*STORAGE-TYPE*) to DESIGNATOR-PATHNAME.

This sets *STORAGE*, *STORAGE-TYPE*, *STORAGE-PATHNAME*, and *CHANGED*.

See *STORAGE*
See *STORAGE-TYPE*
See *STORAGE-PATHNAME*
See *CHANGED*
See DESIGNATOR-PATHNAME")

  ((*ubiquitous-print-table* variable)
   "The pprint-dispatch-table used to write storage objects to file.")

  ((*ubiquitous-read-table* variable)
   "The readtable used to read storage objects from file.")

  ((*ubiquitous-readers* variable)
   "A hash table of functions invoked upon reading a special construct.")

  ((*ubiquitous-char* variable)
   "A string of two characters denoting the start and end of a special construct.")

  ((unknown-reader-type type)
   "Error signalled if an unknown structure type is encountered in the config.

See READER-TYPE
See UBIQUITOUS-READER")

  (reader-type
   "Returns the type that was attempted to be read.

See UNKNOWN-READER-TYPE")

  (ubiquitous-reader
   "Reader function invoked when encountering the first character of *UBIQUITOUS-CHAR*.

This dispatches to the proper reader for the structure's type. If an
unknown type is encountered, an error of type UNKNOWN-READER-TYPE is
signalled.

See UNKNOWN-READER-TYPE")

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

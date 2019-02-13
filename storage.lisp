#|
 This file is a part of ubiquitous
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.ubiquitous)

(defvar *storage* (make-hash-table :test 'equal))
(defvar *storage-type* :lisp)
(defvar *storage-pathname* (designator-pathname :global *storage-type*))

(defgeneric read-storage (type stream))
(defgeneric write-storage (type stream storage))

(defmethod write-storage :around (type pathname storage)
  (call-next-method)
  storage)

(defmacro with-storage ((storage) &body body)
  `(let ((*storage* ,storage))
     ,@body))

(defun lazy-loader (action field &optional value)
  (restore)
  (ecase action
    (:get (field *storage* field value))
    (:set (setf (field *storage* field) value))
    (:remove (remfield *storage* field))))

(defmacro with-local-storage ((designator &key (type '*storage-type*) (storage '#'lazy-loader) (transaction T)) &body body)
  (if transaction
      `(with-transaction (:storage ,storage
                          :type ,type
                          :designator ,designator)
         ,@body)
      `(let* ((*storage* ,storage)
              (*storage-type* ,type)
              (*storage-pathname* ,designator))
         ,@body)))

(define-condition no-storage-file ()
  ((file :initarg :file :accessor file))
  (:default-initargs :file (error "FILE required."))
  (:report (lambda (c s) (format s "Requested storage file ~s does not exist." (namestring (file c))))))

(defgeneric restore (&optional designator type)
  (:method (&optional (designator *storage-pathname*) (type *storage-type*))
    (with-simple-restart (abort "Abort the RESTORE operation.")
      (let ((pathname (designator-pathname designator type)))
        (with-open-file (stream pathname :direction :input :if-does-not-exist NIL)
          (setf *storage* (if stream
                              (read-storage type stream)
                              (restart-case
                                  (progn (signal 'no-storage-file :file pathname)
                                         (make-hash-table :test 'equal))
                                (use-new-storage (value)
                                  :report "Use a new object for storage"
                                  :interactive (lambda () (read *query-io*))
                                  value)))))
        (setf *storage-pathname* pathname)
        (setf *storage-type* type)
        (setf *changed* NIL)))
    *storage*))

(defgeneric offload (&optional designator type storage)
  (:method (&optional (designator *storage-pathname*) (type *storage-type*) (storage *storage*))
    (with-simple-restart (abort "Abort the OFFLOAD operation.")
      (let* ((pathname (designator-pathname designator type))
             (temp (make-pathname :name (format NIL "TMP-~a" (pathname-name pathname))
                                  :defaults pathname)))
        (with-open-file (stream temp :direction :output :if-exists :supersede :if-does-not-exist :create)
          (write-storage *storage-type* stream storage))
        #+(or windows mswindows win32 cormanlisp ccl)
        (when (probe-file pathname) (delete-file pathname))
        (rename-file temp pathname)
        (setf *storage-pathname* pathname)
        (setf *storage-type* type)
        (setf *changed* NIL)))
    *storage*))

(defgeneric destroy (&optional designator type)
  (:method (&optional (designator *storage-pathname*) (type *storage-type*))
    (with-simple-restart (abort "Abort the RESTORE operation.")
      (let ((pathname (designator-pathname designator type)))
        (uiop:delete-file-if-exists pathname)
        (setf *storage* (make-hash-table :test 'equal))
        (setf *storage-pathname* pathname)
        (setf *storage-type* type)
        (setf *changed* NIL)))
    *storage*))

;; Default lisp implementation
(defvar *ubiquitous-print-table* (copy-pprint-dispatch))
(defvar *ubiquitous-read-table* (copy-readtable))
(defvar *ubiquitous-readers* (make-hash-table :test 'eql))
(defvar *ubiquitous-char* "[]")

(define-condition unknown-reader-type (error)
  ((type :initarg :reader-type :initform (error "READER-TYPE required.") :reader reader-type))
  (:report (lambda (c s) (format s "Don't know how to read ~s"
                                 (reader-type c)))))

(progn
  (defun ubiquitous-reader (stream c)
    (declare (ignore c))
    (destructuring-bind (type . args) (read-delimited-list (char *ubiquitous-char* 1) stream T)
      (funcall (or (gethash type *ubiquitous-readers*)
                   (error 'unknown-reader-type :reader-type type))
               args)))
  (set-macro-character (char *ubiquitous-char* 0) #'ubiquitous-reader NIL *ubiquitous-read-table*)
  (set-macro-character (char *ubiquitous-char* 1) (get-macro-character #\)) NIL *ubiquitous-read-table*))

(defun ubiquitous-writer (stream form)
  (pprint-logical-block (stream form :prefix (subseq *ubiquitous-char* 0 1)
                                     :suffix (subseq *ubiquitous-char* 1 2))
    (loop for rest on form
          for item = (pprint-pop)
          do (typecase item
               (list
                (pprint-newline :fill stream)
                (pprint-linear stream item T NIL))
               (T (write item :stream stream)))
             (when (cdr rest) (write-string " " stream)))))

(defmacro define-ubiquitous-writer (type (object &optional (priority 2000)) &body body)
  (let ((stream (gensym "STREAM")))
    `(set-pprint-dispatch ',type (lambda (,stream ,object)
                                   (ubiquitous-writer
                                    ,stream
                                    (list* ',type (progn ,@body))))
                          ,priority *ubiquitous-print-table*)))

(defmacro define-ubiquitous-reader (type (form) &body body)
  `(setf (gethash ',type *ubiquitous-readers*)
         (lambda (,form) ,@body)))

;; Ensure pathnames are being printed as namestrings
(set-pprint-dispatch 'pathname
                     (lambda (stream object)
                       (format stream "#p~s" (namestring object)))
                     1000 *ubiquitous-print-table*)

(set-pprint-dispatch 'string
                     (lambda (stream object)
                       (write-char #\" stream)
                       (unwind-protect
                            (loop for char across object
                                  do (when (or (char= char #\\) (char= char #\"))
                                       (write-char #\\ stream))
                                     (write-char char stream))
                         (write-char #\" stream)))
                     1000 *ubiquitous-print-table*)

(define-ubiquitous-writer hash-table (object 1000)
  (list* (hash-table-test object)
         (loop for k being the hash-keys of object
               for v being the hash-values of object
               collect (list k v))))

;; Some MOP portability.
(defun class-slots (class)
  ()
  #+abcl      (mop:class-slots class)
  #+allegro   (mop:class-slots class)
  #+clisp     (clos:class-slots class)
  #+clozure   (ccl:class-slots class)
  #+cmu       (clos-mop:class-slots class)
  #+ecl       (clos:class-slots class)
  #+lispworks (clos:class-slots class)
  #+mcl       (ccl:class-slots class)
  #+sbcl      (sb-mop:class-slots class)
  #+scl       (clos:class-slots class))

(defun slot-definition-name (slot)
  ()
  #+abcl      (mop:slot-definition-name slot)
  #+allegro   (mop:slot-definition-name slot)
  #+clisp     (clos:slot-definition-name slot)
  #+clozure   (ccl:slot-definition-name slot)
  #+cmu       (clos-mop:slot-definition-name slot)
  #+ecl       (clos:slot-definition-name slot)
  #+lispworks (clos:slot-definition-name slot)
  #+mcl       (ccl:slot-definition-name slot)
  #+sbcl      (sb-mop:slot-definition-name slot)
  #+scl       (clos:slot-definition-name slot))

(define-ubiquitous-writer package (object 0999)
  (list (package-name object)))

(define-ubiquitous-writer structure-class (object 0998)
  (list (class-name object)))

(define-ubiquitous-writer standard-class (object 0997)
  (list (class-name object)))

(define-ubiquitous-writer structure-object (object 0996)
  (list* (class-name (class-of object))
         (loop for slot in (class-slots (class-of object))
               for name = (slot-definition-name slot)
               collect (list name (slot-value object name)))))

(define-ubiquitous-writer standard-object (object 0995)
  (list* (class-name (class-of object))
         (loop for slot in (class-slots (class-of object))
               for name = (slot-definition-name slot)
               collect (list name (slot-value object name)))))

(define-ubiquitous-reader hash-table (form)
  (destructuring-bind (test . vals) form
    (let ((table (make-hash-table :test test)))
      (loop for (key val) in vals
            do (setf (gethash key table) val))
      table)))

(define-ubiquitous-reader standard-object (form)
  (destructuring-bind (type . vals) form
    (let ((object (allocate-instance (find-class type))))
      (loop for (key val) in vals
            do (setf (slot-value object key) val))
      object)))

(define-ubiquitous-reader structure-object (form)
  (destructuring-bind (type . vals) form
    (let ((object (allocate-instance (find-class type))))
      (loop for (key val) in vals
            do (setf (slot-value object key) val))
      object)))

(define-ubiquitous-reader standard-class (form)
  (find-class (first form)))

(define-ubiquitous-reader structure-class (form)
  (find-class (first form)))

(define-ubiquitous-reader package (form)
  (find-package (first form)))

(defmethod read-storage ((type (eql :lisp)) stream)
  (let ((*readtable* *ubiquitous-read-table*))
    (read stream)))

(defmethod write-storage ((type (eql :lisp)) stream storage)
  (let ((*readtable* *ubiquitous-read-table*))
    (write storage
           :stream stream
           :array T
           :base 10
           :case :downcase
           :circle T
           :escape T
           :gensym T
           :length NIL
           :level NIL
           :lines NIL
           :miser-width NIL
           :pprint-dispatch *ubiquitous-print-table*
           :pretty T
           :radix NIL
           :readably T
           :right-margin NIL)))

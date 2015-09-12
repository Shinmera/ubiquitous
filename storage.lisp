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

(define-condition no-storage-file (warning)
  ((file :initarg :file :accessor file))
  (:default-initargs :file (error "FILE required."))
  (:report (lambda (c s) (format s "Requested storage file ~s does not exist." (file c)))))

(defgeneric restore (&optional designator type)
  (:method (&optional (designator *storage-pathname*) (type *storage-type*))
    (with-simple-restart (abort "Abort the RESTORE operation.")
      (let ((pathname (designator-pathname designator type)))
        (with-open-file (stream pathname :direction :input :if-does-not-exist NIL)
          (setf *storage* (if stream
                              (read-storage type stream)
                              (restart-case
                                  (progn (warn 'no-storage-file :file pathname)
                                         (make-hash-table :test 'equal))
                                (use-new-storage (value)
                                  :report "Use a new object for storage"
                                  :interactive (lambda () (read *query-io*))
                                  value)))))
        (setf *storage-pathname* pathname)
        (setf *storage-type* type)))
    *storage*))

(defgeneric offload (&optional designator type storage)
  (:method (&optional (designator *storage-pathname*) (type *storage-type*) (storage *storage*))
    (let* ((pathname (designator-pathname designator type))
           (temp (make-pathname :type "tmp" :defaults pathname)))
      (with-open-file (stream temp :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-storage *storage-type* stream storage))
      (rename-file temp pathname)
      (setf *storage-pathname* pathname)
      (setf *storage-type* type)
      *storage*)))

;; Default lisp implementation
(defvar *ubiquitous-print-table* (copy-pprint-dispatch))
(defvar *ubiquitous-read-table* (copy-readtable))
(defvar *ubiquitous-readers* (make-hash-table :test 'eql))
(defvar *ubiquitous-char* "[]")

(progn
  (defun ubiquitous-reader (stream c)
    (declare (ignore c))
    (destructuring-bind (type . args) (read-delimited-list (char *ubiquitous-char* 1) stream T)
      (funcall (or (gethash type *ubiquitous-readers*)
                   (error "Don't know how to read ~s" type))
               args)))
  (set-macro-character (char *ubiquitous-char* 0) #'ubiquitous-reader T *ubiquitous-read-table*))

(defun ubiquitous-writer (stream form)
  (pprint-logical-block (stream form :prefix (subseq *ubiquitous-char* 0 1) :suffix (subseq *ubiquitous-char* 1))
    (loop for item = (pprint-pop)
          while item
          do (typecase item
               (list
                (pprint-newline :fill stream)
                (pprint-linear stream item T NIL)
                (format stream " "))
               (T (format stream "~s " item))))))

(defmacro define-ubiquitous-writer (type (object) &body body)
  (let ((stream (gensym "STREAM")))
    `(set-pprint-dispatch ',type (lambda (,stream ,object)
                                   (ubiquitous-writer
                                    ,stream
                                    (list* ',type (progn ,@body))))
                          0 *ubiquitous-print-table*)))

(defmacro define-ubiquitous-reader (type (form) &body body)
  `(setf (gethash ',type *ubiquitous-readers*)
         (lambda (,form) ,@body)))

(define-ubiquitous-writer hash-table (object)
  (list* (hash-table-test object)
         (loop for k being the hash-keys of object
               for v being the hash-values of object
               collect (list k v))))

;; Some MOP portability.
(defun class-direct-slots (class)
  ()
  #+abcl      (mop:class-direct-slots class)
  #+allegro   (mop:class-direct-slots class)
  #+clisp     (clos:class-direct-slots class)
  #+clozure   (ccl:class-direct-slots class)
  #+cmu       (clos-mop:class-direct-slots class)
  #+ecl       (clos:class-direct-slots class)
  #+lispworks (clos:class-direct-slots class)
  #+mcl       (ccl:class-direct-slots class)
  #+sbcl      (sb-mop:class-direct-slots class)
  #+scl       (clos:class-direct-slots class))

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

(define-ubiquitous-writer standard-object (object)
  (list* (class-name (class-of object))
         (loop for slot in (class-direct-slots object)
               for name = (slot-definition-name slot)
               collect (list name (slot-value object name)))))

(define-ubiquitous-writer standard-class (object)
  (list (class-name object)))

(define-ubiquitous-writer package (object)
  (list (package-name object)))

(define-ubiquitous-reader hash-table (form)
  (destructuring-bind (test . vals) form
    (let ((table (make-hash-table :test test)))
      (loop for (key val) in vals
            do (setf (gethash key table) val))
      table)))

(define-ubiquitous-reader standard-object (form)
  (destructuring-bind (type . vals) form
    (let ((object (make-instance type)))
      (loop for (key val) in vals
            do (setf (slot-value object key) val))
      object)))

(define-ubiquitous-reader standard-class (form)
  (find-class (first form)))

(define-ubiquitous-reader package (form)
  (find-package (first form)))

(defmethod read-storage ((type (eql :lisp)) stream)
  (let ((*readtable* *ubiquitous-read-table*))
    (read stream)))

(defmethod write-storage ((type (eql :lisp)) stream storage)
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
         :right-margin NIL))

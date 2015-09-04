#|
 This file is a part of ubiquitous
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.ubiquitous)

(defvar *commit* T)

(defgeneric value (&rest path)
  (:method (&rest path)
    (loop with object = *storage*
          for branch in path
          do (multiple-value-bind (value found) (field object branch)
               (if found
                   (setf object value)
                   (return (values NIL NIL))))
          finally (return (values object T)))))

(defgeneric (setf value) (value &rest path)
  (:method (value &rest path)
    (cond (path
           (loop with object = *storage*
                 for cons on path
                 for branch = (car cons)
                 while (cdr cons)
                 do (multiple-value-bind (value found) (field object branch)
                      (if found
                          (setf object value)
                          (setf object (augment object branch (cadr cons)))))
                 finally (setf (field object branch) value)))
          (T
           (setf *storage* value)))
    (when *commit*
      (offload))
    value))

(defgeneric remvalue (&rest path)
  (:method (&rest path)
    (let (found)
      (cond (path
             (loop with object = *storage*
                   for cons on path
                   for branch = (car cons)
                   while (cdr cons)
                   do (multiple-value-bind (value found) (field object branch)
                        (if found
                            (setf object value)
                            (return)))
                   finally (setf found (nth-value 1 (remfield object branch)))))
            (T
             (setf *storage* (make-hash-table :test 'equal))
             (setf found T)))
      (when (and found *commit*)
        (offload))
      (values *storage* found))))

(defgeneric defaulted-value (default &rest path)
  (:method (default &rest path)
    (multiple-value-bind (value found) (apply #'value path)
      (if found
          value
          (apply #'(setf value) default path)))))

(defgeneric call-with-transaction (function &key storage type designator)
  (:method (function &key storage type designator)
    (let ((*commit* NIL)
          (*storage* (or storage *storage*))
          (*storage-type* (or type *storage-type*))
          (*storage-pathname* (or designator *storage-pathname*)))
      (unwind-protect
           (funcall function)
        (offload)))))

(defmacro with-transaction ((&key storage type designator) &body body)
  `(call-with-transaction
    (lambda () ,@body)
    :storage ,storage
    :type ,type
    :designator ,designator))

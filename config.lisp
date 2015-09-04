#|
 This file is a part of ubiquitous
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.ubiquitous)

(defun value (&rest path)
  (loop with object = *storage*
        for branch in path
        do (multiple-value-bind (value found) (field object branch)
             (if found
                 (setf object value)
                 (return (values NIL NIL))))
        finally (return (values object T))))

(defun (setf value) (value &rest path)
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
  (offload)
  value)

(defun remvalue (&rest path)
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
    (offload)
    (values *storage* found)))

(defun defaulted-value (default &rest path)
  (multiple-value-bind (value found) (apply #'value path)
    (if found
        value
        (apply #'(setf value) default path))))

#|
 This file is a part of ubiquitous
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.ubiquitous)

(defvar *nothing* (gensym))

(defgeneric field (object field &optional default))
(defgeneric (setf field) (value object field))
(defgeneric remfield (object field))
(defgeneric augment (object field secondary))

(defmethod field ((object hash-table) field &optional default)
  (let ((value (gethash field object *nothing*)))
    (if (eq value *nothing*)
        (values default NIL)
        (values value T))))

(defmethod field ((object vector) (field integer) &optional default)
  (if (< field (length object))
      (values (elt object field) T)
      (values default NIL)))

(defmethod field ((object list) (field integer) &optional default)
  (let ((cons (nthcdr field object)))
    (if cons
        (values (car cons) T)
        (values default NIL))))

(defmethod field ((object list) (field symbol) &optional default)
  (let ((cons (assoc field object)))
    (if cons
        (values (cdr cons) T)
        (values default NIL))))

(defmethod field ((object list) (field string) &optional default)
  (let ((cons (assoc field object :test #'string=)))
    (if cons
        (values (cdr cons) T)
        (values default NIL))))

(defmethod field ((object standard-object) (field symbol) &optional default)
  (if (slot-exists-p object field)
      (values (slot-value object field) T)
      (values default NIL)))

(defmethod field ((object symbol) (field symbol) &optional default)
  (let ((value (get object field *nothing*)))
    (if (eq value *nothing*)
        (values default NIL)
        (values value T))))

(defmethod field ((object null) field &optional default)
  (values default NIL))

(defmethod (setf field) :around (value object field)
  (call-next-method)
  value)

(defmethod (setf field) (value (object hash-table) field)
  (setf (gethash field object) value))

(defmethod (setf field) (value (object vector) (field integer))
  (setf (elt object field) value))

(defmethod (setf field) (value (object list) (field integer))
  (setf (nth field object) value))

(defmethod (setf field) (value (object list) (field symbol))
  (let ((cons (assoc field object)))
    (if cons
        (setf (cdr cons) value)
        (setf (cdr object) (cons value (cdr object))))))

(defmethod (setf field) (value (object list) (field string))
  (let ((cons (assoc field object :test #'string=)))
    (if cons
        (setf (cdr cons) value)
        (setf (cdr object) (cons value (cdr object))))))

(defmethod (setf field) (value (object standard-object) (field symbol))
  (setf (slot-value object field) value))

(defmethod (setf field) (value (object symbol) (field symbol))
  (setf (get object field) value))

(defmethod remfield :around (object field)
  (if (call-next-method)
      (values object T)
      (values object NIL)))

(defmethod remfield ((object hash-table) field)
  (remhash field object))

(defmethod remfield ((object list) (field integer))
  (let ((prev (when (< 0 field)
                (nthcdr (1- field) object))))
    (cond ((and (not (cdr prev)) (< 0 field))
           NIL)
          (prev
           (setf (cdr prev) (cddr prev))
           T)
          (T
           (setf (car object) (cadr object)
                 (cdr object) (cddr object))
           T))))

(defmethod remfield ((object list) (field symbol))
  (let ((cons (loop for cons on object
                    when (eql (caar cons) field)
                    do (return cons))))
    (when cons
      (setf (car cons) (cadr cons)
            (cdr cons) (cddr cons))     
      T)))

(defmethod remfield ((object list) (field string))
  (let ((cons (loop for cons on object
                    when (equal (caar cons) field)
                    do (return cons))))
    (when cons
      (setf (car cons) (cadr cons)
            (cdr cons) (cddr cons))
      T)))

(defmethod remfield ((object symbol) (field symbol))
  (remprop object field))

(defmethod augment (object field (secondary symbol))
  (setf (field object field) (make-hash-table :test 'eql)))

(defmethod augment (object field (secondary integer))
  (setf (field object field) (make-hash-table :test 'eql)))

(defmethod augment (object field (secondary character))
  (setf (field object field) (make-hash-table :test 'eql)))

(defmethod augment (object field (secondary string))
  (setf (field object field) (make-hash-table :test 'equal)))

(defmethod augment (object field (secondary bit-vector))
  (setf (field object field) (make-hash-table :test 'equal)))

(defmethod augment (object field (secondary pathname))
  (setf (field object field) (make-hash-table :test 'equal)))

(defmethod augment (object field (secondary array))
  (setf (field object field) (make-hash-table :test 'equalp)))

(defmethod augment (object field (secondary structure-object))
  (setf (field object field) (make-hash-table :test 'equalp)))

(defmethod augment (object field (secondary hash-table))
  (setf (field object field) (make-hash-table :test 'equalp)))

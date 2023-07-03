(in-package #:org.shirakumo.ubiquitous)

(defvar *locked-objects* (make-hash-table :test 'eq))
(defvar *table-lock* (bt:make-lock))
(defvar *superior-lock* NIL)

(defun await (object)
  (loop while (gethash object *locked-objects*)
        do (bt:release-lock *table-lock*)
           (bt:thread-yield)
           (bt:acquire-lock *table-lock*)))

(defun lock-object (object)
  (bt:with-lock-held (*table-lock*)
    (await object)
    (setf (gethash object *locked-objects*) T)))

(defun unlock-object (object)
  (bt:with-lock-held (*table-lock*)
    (remhash object *locked-objects*)))

(defun call-with-object-locked (function object)
  (unwind-protect
       (progn (lock-object object)
              (funcall function))
    (unlock-object object)))

(defmacro with-object-locked ((object) &body body)
  `(call-with-object-locked (lambda () ,@body) ,object))

(defmacro superior-lock-next-method-call (object)
  `(if *superior-lock*
       (call-next-method)
       (with-object-locked (,object)
         (let ((*superior-lock* T))
           (call-next-method)))))

;; We just lock the whole storage. Since the paths are likely not to be
;; immensely deep, it is better to reduce locking to a minimum. Therefore
;; while a global lock will result in only a single accessor at a time,
;; it should instead make the actual operation much faster to perform,
;; thus increasing performance overall.
;;
;; If we did not lock the whole storage, we would have to lock each
;; individual access to an object through FIELD, which would result in
;; a couple of locking operations every time, and would still result in
;; exclusive access on a per-object basis, including the base storage on
;; initial entry. Thus, it is much more likely to be less efficient than
;; a storage lock.
;;
;; The deficit of this approach is that the user may use the FIELD
;; functions directly outside of the VALUE functions, which might result
;; in instability and unexpected behaviour. To remedy this, we introduce
;; a special variable *SUPERIOR-LOCK* which should only be T if we are
;; globally locking. If it is NIL, the FIELD functions will themselves
;; lock on the object they are accessing. This also helps in the contagion
;; between OFFLOAD and (SETF VALUE), the latter of which calls the former.
(defmethod value :around (&rest path)
  (declare (ignore path))
  (superior-lock-next-method-call *storage*))

(defmethod (setf value) :around (value &rest path)
  (declare (ignore value path))
  (superior-lock-next-method-call *storage*))

(defmethod remvalue :around (&rest path)
  (declare (ignore path))
  (superior-lock-next-method-call *storage*))

(defmethod call-with-transaction :around (function &key storage type designator)
  (declare (ignore type designator))
  (superior-lock-next-method-call (or storage *storage*)))

(defmethod field :around (object field &optional default)
  (declare (ignore default))
  (superior-lock-next-method-call object))

;; Unfortunately due to specifier ambiguity we are overriding, so we
;; need to be careful to reimplement the behaviour for (SETF FIELD)
;; and (REMFIELD).
(defmethod (setf field) :around (value object field)
  (superior-lock-next-method-call object)
  value)

(defmethod remfield :around (object field)
  (if (superior-lock-next-method-call object)
      (values object T)
      (values object NIL)))

(defmethod offload :around (&optional designator type (storage *storage*))
  (declare (ignore designator type))
  (superior-lock-next-method-call storage))

(defmethod restore :around (&optional designator type)
  (declare (ignore designator type))
  (superior-lock-next-method-call *storage*))

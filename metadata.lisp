#|
 This file is a part of ubiquitous
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.ubiquitous)

(defvar *metadata-version* 1.0)
(defvar *metadata-prefix* "; meta ")

(define-condition metadata-condition (condition)
  ())

(define-condition unknown-version (warning metadata-condition)
  ((version :initarg :version :initform (error "VERSION required.") :reader version))
  (:report (lambda (c s) (format s "The version ~s found in the config file is not known.~%~
                                    This could lead to an incorrectly read configuration."
                                 (version c)))))

(define-condition bad-configuration-package (error metadata-condition)
  ((package :initarg :package-name :initform (error "PACKAGE-NAME required.") :reader bad-package-name))
  (:report (lambda (c s) (format s "The configuration metadata specifies a package named ~s.~%~
                                    However, no package of this name exists in the current system."
                                 (bad-package-name c)))))

(define-condition bad-metadata-header (error metadata-condition)
  ((header :initarg :header :initform (error "HEADER required.") :reader header))
  (:report (lambda (c s) (format s "The configuration header is malformed and cannot be parsed:~%  ~s"
                                 (header c)))))

(defun find-metadata-package (name)
  (restart-case
      (or (find-package name)
          (error 'bad-configuration-package :package-name name))
    (use-value (package)
      :report "Supply a different package."
      :interactive (lambda () (list (read *query-io*)))
      (etypecase package
        ((or symbol string) (find-metadata-package package))
        (package package)))
    (continue ()
      :report (lambda (s) (format s "Continue with the package ~s" (package-name *package*)))
      *package*)))

(defun check-metadata (meta)
  (unless (listp meta)
    (error 'bad-metadata-header :header meta))
  (unless (evenp (length meta))
    (error 'bad-metadata-header :header meta))
  (loop for (key val) on meta by #'cddr
        do (unless (symbolp key)
             (error 'bad-metadata-header :header meta))))

(defun process-metadata (meta)
  (restart-case
      (when meta
        (check-metadata meta)
        (destructuring-bind (&key version package &allow-other-keys) meta
          (unless version
            (error 'bad-metadata-header :header meta))
          (when (<= 1.0 version)
            (when package
              (setf *package* (find-metadata-package package))))
          (case version
            (1.0)
            (T (warn 'unknown-version :version version)))))
    (use-value (meta)
      :report "Supply different metadata."
      :interactive (lambda () (list (read *query-io*)))
      (process-metadata meta))
    (continue ()
      :report "Ignore the metadata and attempt to continue without it.")))

(defun generate-metadata ()
  (list :version *metadata-version*
        :package (package-name *package*)))

(defun prefix-p (prefix string)
  (and (< (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun maybe-read-metadata (stream)
  (let ((first (peek-char NIL stream NIL)))
    (when (eql #\; first)
      (let ((line (read-line stream))
            (*package* #.*package*))
        (when (prefix-p *metadata-prefix* line)
          (handler-case (read-from-string line T NIL :start (length *metadata-prefix*))
            (error (e)
              (declare (ignore e))
              (error 'bad-metadata-header :header line))))))))

(defun print-metadata (stream &optional (metadata (generate-metadata)))
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-length* NIL)
          (*print-miser-width* NIL)
          (*print-right-margin* NIL)
          (*print-readably* NIL)
          (*package* #.*package*))
      (format stream "~&~a~s~%" *metadata-prefix* metadata))))

(defmacro with-processed-metadata (meta &body body)
  `(let ((*package* *package*))
     (process-metadata ,meta)
     ,@body))

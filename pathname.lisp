#|
 This file is a part of ubiquitous
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.ubiquitous)

(defun config-directory ()
  #+(or windows win32 mswindows)
  (merge-pathnames (make-pathname :directory '(:relative "AppData" "Local" "common-lisp" "ubiquitous"))
                   (user-homedir-pathname))
  #-(or windows win32 mswindows)
  (merge-pathnames (make-pathname :directory '(:relative ".config" "common-lisp" "ubiquitous"))
                   (user-homedir-pathname)))

(defun config-pathname (type)
  (make-pathname :type (format NIL "conf.~(~a~)" type) :defaults (config-directory)))

(defun split-package-name (package)
  (let ((parts ())
        (buffer (make-string-output-stream)))
    (flet ((maybe-add-part ()
             (let ((part (get-output-stream-string buffer)))
               (when (string/= part "")
                 (push part parts)))))
      (loop for char across (etypecase package
                              (package (package-name package))
                              (string package))
            do (case char ;; FIXME: potentially add #\. as well.
                 ((#\/) (maybe-add-part))
                 (T (write-char (char-downcase char) buffer)))
            finally (maybe-add-part))
      (nreverse parts))))

(defgeneric package-directory (package))

(defmethod package-directory ((name symbol))
  (package-directory (symbol-name name)))

(defmethod package-directory ((name string))
  (package-directory (or (find-package name)
                         (error "No package named ~s found." name))))

(defmethod package-directory ((package (eql (find-package "CL"))))
  (error "Using the CL package for storage would only lead to hard-to-debug collisions."))

(defmethod package-directory ((package null))
  (config-directory))

(defmethod package-directory ((package (eql (find-package "KEYWORD"))))
  (config-directory))

(defmethod package-directory ((package package))
  (merge-pathnames
   (make-pathname :directory (list* :relative (split-package-name package)))
   (config-directory)))

(defgeneric designator-pathname (designator type))

(defmethod designator-pathname ((designator pathname) type)
  (cond ((eql :absolute (car (pathname-directory designator)))
         designator)
        (T
         (merge-pathnames designator (config-directory)))))

(defmethod designator-pathname ((designator symbol) type)
  (make-pathname :name (string-downcase designator)
                 :type (pathname-type (config-pathname type))
                 :defaults (package-directory (symbol-package designator))))

(defmethod designator-pathname ((designator string) type)
  (merge-pathnames (pathname designator) (config-pathname type)))

(defmethod designator-pathname :around (designator type)
  (ensure-directories-exist
   (call-next-method)))

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

(defgeneric designator-pathname (designator type))

(defmethod designator-pathname ((designator pathname) type)
  (cond ((eql :absolute (car (pathname-directory designator)))
         designator)
        (T
         (merge-pathnames designator (config-directory)))))


(defun split (string separator)
  "Returns a list of strings, splitting by given character"
  (check-type string string)
  (check-type separator character)
  (loop for i = 0 then (1+ j)
        as j = (position separator string
                         :start i
                         :test #'char=)
        collect (subseq string i j)
        while j))


(defmethod designator-pathname ((designator symbol) type)
  (cond ((eq (symbol-package designator) (find-package '#:cl))
         (error "Do not use symbols from the CL package for storage!"))
        ((or (eq (symbol-package designator) (find-package '#:keyword))
             (eq (symbol-package designator) NIL))
         (make-pathname :name (string-downcase designator) :defaults (config-pathname type)))
        (T
         (merge-pathnames (make-pathname :directory (list* :relative (split
                                                                      (string-downcase
                                                                       (package-name
                                                                        (symbol-package designator)))
                                                                      #\/))
                                         :name (string-downcase designator))
                          (config-pathname type)))))

(defmethod designator-pathname ((designator string) type)
  (merge-pathnames (pathname designator) (config-pathname type)))

(defmethod designator-pathname :around (designator type)
  (ensure-directories-exist
   (call-next-method)))

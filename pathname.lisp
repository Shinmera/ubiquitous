#|
 This file is a part of ubiquitous
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.ubiquitous)

(defun getenv (x)
  (declare (ignorable x))
  #+(or abcl clasp clisp ecl xcl) (ext:getenv x)
  #+allegro (sys:getenv x)
  #+clozure (ccl:getenv x)
  #+cmucl (unix:unix-getenv x)
  #+scl (cdr (assoc x ext:*environment-list* :test #'string=))
  #+cormanlisp
  (let* ((buffer (ct:malloc 1))
         (cname (ct:lisp-string-to-c-string x))
         (needed-size (win:getenvironmentvariable cname buffer 0))
         (buffer1 (ct:malloc (1+ needed-size))))
    (prog1 (if (zerop (win:getenvironmentvariable cname buffer1 needed-size))
               nil
               (ct:c-string-to-lisp-string buffer1))
      (ct:free buffer)
      (ct:free buffer1)))
  #+gcl (system:getenv x)
  #+genera nil
  #+lispworks (lispworks:environment-variable x)
  #+mcl (ccl:with-cstrs ((name x))
          (let ((value (_getenv name)))
            (unless (ccl:%null-ptr-p value)
              (ccl:%get-cstring value))))
  #+mkcl (#.(or (find-symbol 'getenv :si) (find-symbol 'getenv :mk-ext) '((lambda (x) nil))) x)
  #+sbcl (sb-ext:posix-getenv x)
  #-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
  NIL)

(defun config-directory ()
  (flet ((or* (&rest args)
           (loop for arg in args do
                 (typecase arg
                   (null)
                   (pathname (return arg))
                   (string (when (string/= "" arg)
                             (return (make-pathname :directory arg))))))))
    (let ((config (or* (getenv "UBIQUITOUS_HOME")
                       #+(or windows win32 mswindows)
                       (getenv "APPDATA")
                       (getenv "XDG_CONFIG_HOME")
                       (merge-pathnames (make-pathname :directory '(:relative ".config")) (user-homedir-pathname)))))
      (merge-pathnames (make-pathname :directory '(:relative "common-lisp" "ubiquitous")) config))))

(defun config-pathname (type)
  (make-pathname :type #-clisp (format NIL "conf.~(~a~)" type) #+clisp "lisp" :defaults (config-directory)))

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

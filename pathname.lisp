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

(defun parse-native-namestring (namestring &key (as :file) junk-allowed)
  #+windows (parse-dos-namestring namestring :as as :junk-allowed junk-allowed)
  #+unix (parse-unix-namestring namestring :as as :junk-allowed junk-allowed)
  #-(or windows unix)
  (let ((path (parse-namestring namestring NIL *default-pathname-defaults* :junk-allowed junk-allowed)))
    (if (and (eql :directory as)
             (or (pathname-name path) (pathname-type path)))
        (make-pathname :directory (append (pathname-directory path) (list (format NIL "~a~@[.~a~]" (pathname-name path) (pathname-type path)))))
        path)))

(defun parse-unix-namestring (namestring &key (as :file) junk-allowed)
  (if (string= "" namestring)
      #p""
      (let ((base (case (char namestring 0)
                    (#\~ '(:home :absolute))
                    (#\/ '(:absolute))
                    (T '(:relative))))
            (buf (make-string-output-stream))
            (name NIL)
            (type NIL))
        (flet ((push-file ()
                 (let* ((leftover (get-output-stream-string buf))
                        (dot (position #\. leftover :from-end T)))
                   (when (string/= "" leftover)
                     (case dot
                       ((0 NIL) (setf name leftover))
                       (T (setf name (subseq leftover 0 dot)
                                type (subseq leftover (1+ dot))))))))
               (push-dir ()
                 (let* ((dirname (get-output-stream-string buf)))
                   (cond ((string= "" dirname))
                         ((string= "." dirname))
                         ((string= ".." dirname) (push :back base))
                         (T (push dirname base))))))
          (loop for i from (if (eql :relative (first base)) 0 1) below (length namestring)
                for char = (char namestring i)
                do (case char
                     (#\/ (push-dir))
                     (#\Nul (unless junk-allowed
                              (cerror "Ignore the character" "Illegal character ~c in namestring:~%  ~a"
                                      char namestring)))
                     (T (write-char char buf)))
                finally (ecase as
                          (:file (push-file))
                          (:directory (push-dir)))))
        (make-pathname :name name :type type :directory (unless (equal base '(:relative)) (reverse base))))))

(defun parse-dos-namestring (namestring &key (as :file) junk-allowed)
  (if (string= "" namestring)
      #p""
      (let ((directory '(:relative))
            (buf (make-string-output-stream))
            (device NIL)
            (name NIL)
            (type NIL)
            (base #p"")
            (start 0))
        (let ((colon (position #\: namestring)))
          (when (and colon (< 0 colon)
                     (loop for i from 0 below colon always (alpha-char-p (char namestring i))))
            (setf device (subseq namestring 0 colon))
            (setf start (1+ colon))))
        (case (char namestring start)
          ((#\\ #\/)
           (setf directory '(:absolute))
           (incf start)))
        (flet ((push-file ()
                 (let* ((leftover (get-output-stream-string buf))
                        (dot (position #\. leftover :from-end T)))
                   (when (string/= "" leftover)
                     (case dot
                       ((0 NIL) (setf name leftover))
                       (T (setf name (subseq leftover 0 dot)
                                type (subseq leftover (1+ dot))))))))
               (push-dir ()
                 (let* ((dirname (get-output-stream-string buf)))
                   (cond ((string= "" dirname))
                         ((string= "." dirname))
                         ((string= ".." dirname)
                          (push :back directory))
                         (T (push dirname directory))))))
          (loop for i from start below (length namestring)
                for char = (char namestring i)
                do (case char
                     ((#\\ #\/) (push-dir))
                     ((#\< #\> #\: #\" #\| #\? #\* #\Nul)
                      (unless junk-allowed
                        (cerror "Ignore the character" "Illegal character ~c in namestring:~%  ~a"
                                char namestring)))
                     (T (write-char char buf)))
                finally (ecase as
                          (:file (push-file))
                          (:directory (push-dir)))))
        (merge-pathnames (make-pathname :name name :type type :device device :directory (unless (equal directory '(:relative)) (reverse directory)))
                         base))))

(defun config-directory ()
  (flet ((or* (&rest args)
           (loop for arg in args do
             (typecase arg
               (null)
               (pathname (return arg))
               (string (when (string/= "" arg)
                         (return (parse-native-namestring arg :as :directory :junk-allowed T))))))))
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

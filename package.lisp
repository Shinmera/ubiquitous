#|
 This file is a part of ubiquitous
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:ubiquitous
  (:nicknames #:org.shirakumo.ubiquitous)
  (:use #:cl)
  ;; accessor.lisp
  (:export
   #:field
   #:remfield
   #:augment)
  ;; config.lisp
  (:export
   #:*commit*
   #:value
   #:remvalue
   #:defaulted-value
   #:call-with-transaction
   #:with-transaction)
  ;; pathname.lisp
  (:export
   #:config-directory
   #:config-pathname
   #:designator-pathname)
  ;; storage.lisp
  (:export
   #:*storage*
   #:*storage-type*
   #:*storage-pathname*
   #:read-storage
   #:write-storage
   #:with-storage
   #:restore
   #:offload
   #:define-ubiquitous-writer
   #:define-ubiquitous-reader))

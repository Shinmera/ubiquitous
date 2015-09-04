#|
 This file is a part of ubiquitous
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem ubiquitous
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A library providing a universal application configuration mechanism."
  :homepage "https://github.com/Shinmera/ubiquitous"
  :serial T
  :components ((:file "package")
               (:file "pathname")
               (:file "accessor")
               (:file "storage")
               (:file "config")
               (:file "documentation"))
  :build-operation asdf:monolithic-concatenate-source-op
  :build-pathname "ubiquitous"
  :depends-on ())

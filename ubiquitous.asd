#|
 This file is a part of ubiquitous
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem ubiquitous
  :version "2.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A library providing a universal application configuration mechanism."
  :homepage "https://github.com/Shinmera/ubiquitous"
  :serial T
  :components ((:file "package")
               (:file "pathname")
               (:file "accessor")
               (:file "metadata")
               (:file "storage")
               (:file "config")
               (:file "documentation"))
  :build-operation asdf:monolithic-concatenate-source-op
  :build-pathname "ubiquitous"
  :depends-on ())

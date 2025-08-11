(asdf:defsystem ubiquitous
  :version "2.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library providing a universal application configuration mechanism."
  :homepage "https://shinmera.com/project/ubiquitous"
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

(asdf:defsystem ubiquitous-concurrent
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An extension to ubiquitous providing concurrency support."
  :homepage "https://github.com/Shinmera/ubiquitous"
  :serial T
  :components ((:file "concurrent"))
  :depends-on (:ubiquitous
               :bordeaux-threads))

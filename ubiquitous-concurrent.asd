#|
 This file is a part of ubiquitous
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem ubiquitous-concurrent
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An extension to ubiquitous providing concurrency support."
  :homepage "https://github.com/Shinmera/ubiquitous"
  :serial T
  :components ((:file "concurrent"))
  :depends-on (:ubiquitous
               :bordeaux-threads))

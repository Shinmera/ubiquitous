#|
 This file is a part of ubiquitous
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem ubiquitous-concurrent
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An extension to ubiquitous providing concurrency support."
  :homepage "https://github.com/Shinmera/ubiquitous"
  :serial T
  :components ((:file "concurrent"))
  :depends-on (:ubiquitous
               :bordeaux-threads))

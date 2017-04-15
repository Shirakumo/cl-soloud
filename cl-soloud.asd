#|
 This file is a part of cl-out123
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem cl-soloud
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Bindings to SoLoud, a multi-platform, multi-backend, minimal dependencies sound mixing and output library"
  :homepage "https://github.com/Shirakumo/cl-soloud"
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "toolkit")
               (:file "filter")
               (:file "source")
               (:file "soloud")
               (:file "bus")
               (:file "mp3")
               (:file "documentation"))
  :depends-on (:alexandria
               :cffi
               :trivial-features
               :trivial-garbage
               :trivial-indent
               :documentation-utils
               :cl-mpg123))

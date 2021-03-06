#|
 This file is a part of cl-out123
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem cl-soloud
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Bindings to SoLoud, a multi-platform, multi-backend, minimal dependencies sound mixing and output library"
  :homepage "https://Shirakumo.github.io/cl-soloud/"
  :bug-tracker "https://github.com/Shirakumo/cl-soloud/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-soloud.git")
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

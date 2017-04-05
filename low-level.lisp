#|
 This file is a part of cl-soloud
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.soloud.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(define-foreign-library libsoloud
  (:darwin (:or "libsoloud.dylib" "libsoloud.so"
                #+X86 "mac32-libsoloud.dylib"
                #+X86-64 "mac64-libsoloud.dylib"))
  (:unix (:or "libsoloud.so"
              #+X86 "lin32-libsoloud.so"
              #+X86-64 "lin64-libsoloud.so"))
  (:windows (:or "out123.dll"
                 #+X86 "win32-libsoloud.dll"
                 #+X86-64 "win64-libsoloud.dll"))
  (t (:default "soloud")))

(use-foreign-library libsoloud)


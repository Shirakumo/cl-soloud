#|
 This file is a part of cl-soloud
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:cl-soloud-cffi
  (:nicknames #:org.shirakumo.fraf.soloud.cffi)
  (:use #:cl #:cffi)
  ;; low-level.lisp
  (:shadow :open :close :continue)
  (:export
   #:*static*
   #:libsoloud))

(defpackage #:cl-soloud
  (:nicknames #:org.shirakumo.fraf.soloud)
  (:use #:cl #:cffi)
  )

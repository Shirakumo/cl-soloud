#|
 This file is a part of cl-soloud
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.soloud)

(defvar *c-object-table* (tg:make-weak-hash-table :test 'eql :weakness :value))

(defclass c-backed-object ()
  ((handle :initform NIL :accessor handle)))

(defmethod initialize-instance :after ((c-backed-object c-backed-object) &key)
  (let ((handle (create-handle c-backed-object)))
    (when (cffi:null-pointer-p handle)
      (error "Failed to create ~a handle." c-backed-object))
    (setf (handle soloud) handle)
    (tg:finalize handle (destroy-handle c-backed-object handle))
    (setf (gethash (cffi:pointer-address handle) *c-object-table*) c-backed-object)))

(defmethod pointer->object ((pointer integer))
  (gethash integer *c-object-table*))

(defmethod pointer->object (pointer)
  (gethash (cffi:pointer-address pointer) *c-object-table*))

(defmacro with-callback-handling ((instance &optional default) &body body)
  `(handler-case
       (let ((,instance (pointer->object ,instance)))
         (if ,instance
             ,@body
             ,default))
     (error (err)
       (format T "~&! Error in callback: ~a~%" err)
       ,default)))

(defun find-cffi-symbol (temp fill)
  (find-symbol (with-output-to-string (o)
                 (loop for c across (string temp)
                       do (if (eql c #\_)
                              (write-sequence (string fill) o)
                              (write-char c o))))
               '#:org.shirakumo.fraf.soloud.cffi))

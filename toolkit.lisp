#|
 This file is a part of cl-soloud
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.soloud)

(defvar *c-object-table* (tg:make-weak-hash-table :test 'eql :weakness :value))

(defclass c-backed-object ()
  ((handle :initarg :handle :initform NIL :accessor handle)))

(defmethod initialize-instance :after ((c-backed-object c-backed-object) &key handle)
  (unless handle
    (let ((handle (create-handle c-backed-object)))
      (when (cffi:null-pointer-p handle)
        (error "Failed to create ~a handle." c-backed-object))
      (setf (handle c-backed-object) handle)
      (tg:finalize handle (destroy-handle c-backed-object handle))
      (setf (gethash (cffi:pointer-address handle) *c-object-table*) c-backed-object))))

(defmethod pointer->object ((pointer integer))
  (gethash integer *c-object-table*))

(defmethod pointer->object (pointer)
  (gethash (cffi:pointer-address pointer) *c-object-table*))

(defmacro with-callback-handling ((instance &optional default (error default)) &body body)
  `(handler-case
       (let ((,instance (pointer->object ,instance)))
         (if ,instance
             ,@body
             ,default))
     (error (err)
       (format T "~&! Error in callback: ~a~%" err)
       ,error)))

(defun find-cffi-symbol (temp fill)
  (let ((symb (with-output-to-string (o)
                (loop for c across (string temp)
                      do (if (eql c #\_)
                             (write-sequence (string fill) o)
                             (write-char c o))))))
    (or (find-symbol symb '#:org.shirakumo.fraf.soloud.cffi)
        (error "No such symbol ~a found in CL-SOLOUD-CFFI" symb))))

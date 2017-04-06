#|
 This file is a part of cl-soloud
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.soloud)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-cffi-symbol (temp fill)
    (find-symbol (with-output-to-string (o)
                   (loop for c across (string temp)
                         do (if (eql c #\_)
                                (write-sequence (string fill) o)
                                (write-char c o))))
                 '#:org.shirakumo.fraf.soloud.cffi)))

(defmacro define-source (class &optional (name class))
  (flet ((fun (symb &rest args)
           (list* (or (find-cffi-symbol symb name)
                      (error "No such symbol ~a for ~a" symb name))
                  `(handle ,name)
                  args)))
    `(progn
       (defclass ,class (source)
         ())
       
       (defmethod (setf volume) (value (,name ,class) &key fade)
         ,(fun 'set-_-volume 'value))
       
       (defmethod (setf looping) (value (,name ,class))
         ,(fun 'set-_-looping '(if value 1 0)))
       
       (defmethod (setf min-max-distance) (value (,name ,class))
         (destructuring-bind (min max) value
           ,(fun 'set-_-3d-min-max-distance 'min 'max)))
       
       (defmethod (setf attenuation) (value (,name ,class))
         (destructuring-bind (model rolloff) value
           ,(fun 'set-_-3d-attenuation 'model 'rolloff)))
       
       (defmethod (setf doppler-factor) (value (,name ,class))
         ,(fun 'set-_-3d-doppler-factor 'value))
       
       (defmethod (setf 3d-processing) (value (,name ,class))
         ,(fun 'set-_-3d-processing '(if value 1 0)))
       
       (defmethod (setf listener-relative) (value (,name ,class))
         ,(fun 'set-_-3d-listener-relative '(if value 1 0)))
       
       (defmethod (setf distance-delay) (value (,name ,class))
         ,(fun 'set-_-3d-distance-delay '(if value 1 0)))
       
       (defmethod (setf collider) (value (,name ,class))
         (if (listp value)
             (destructuring-bind (collider user-data) value
               ,(fun 'set-_-3d-collider* '(handle collider) 'user-data))
             ,(fun 'set-_-3d-collider '(handle value))))
       
       (defmethod (setf attenuator) (value (,name ,class))
         ,(fun 'set-_-3d-attenuator '(handle value)))
       
       (defmethod (setf inaudible-behavior) (value (,name ,class))
         (destructuring-bind (must-tick kill) value
           ,(fun 'set-_-3d-inaudible-behavior '(if must-tick 1 0) '(if kill 1 0))))
       
       (defmethod (setf filter) ((filter filter) (,name ,class) id)
         (check-type id (integer 0 #.cl-soloud-cffi:*max-filters*))
         ,(fun 'set-_-filter 'id '(handle filter)))
       
       (defmethod stop ((,name ,class))
         ,(fun 'stop-_)))))

(define-source wav-source wav)

(defmethod load ((source wav-source) file)
  (cl-soloud-cffi:load-wav
   (handle source) (uiop:native-namestring file)))

(defmethod load-mem ((source wav-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-wav-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-source wav-stream-source wav-stream)

(defmethod load ((source wav-stream-source) file)
  (cl-soloud-cffi:load-wav-stream
   (handle source) (uiop:native-namestring file)))

(defmethod load-mem ((source wav-stream-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-wav-stream-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-source speech-source speech)

(defmethod load ((source speech-source) text)
  (cl-soloud-cffi:set-speech-text
   (handle source) text))

(define-source sfxr-source sfxr)

(defmethod load ((source sfxr-source) file)
  (cl-soloud-cffi:load-sfxr-params
   (handle source) (uiop:native-namestring file)))

(defmethod load ((source sfxr-source) (preset symbol))
  (cl-soloud-cffi:load-sfxr-preset
   (handle source) preset (random (ash 1 (* 8 (cffi:foreign-type-size :int))))))

(defmethod load-mem ((source sfxr-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-sfxr-params-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-source monotone-source monotone)

(defmethod load ((source monotone-source) file)
  (cl-soloud-cffi:load-monotone
   (handle source) (uiop:native-namestring file)))

(defmethod load-mem ((source monotone-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-monotone-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-source ted-sid-source ted-sid)

(defmethod load ((source ted-sid-source) file)
  (cl-soloud-cffi:load-ted-sid
   (handle source) (uiop:native-namestring file)))

(defmethod load-mem ((source ted-sid-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-ted-sid-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))



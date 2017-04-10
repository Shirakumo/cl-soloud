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

(defmacro define-internal-source (class &optional (name class) superclasses)
  (flet ((fun (symb &rest args)
           (list* (or (find-cffi-symbol symb name)
                      (error "No such symbol ~a for ~a" symb name))
                  `(handle ,name)
                  args)))
    `(progn
       (defclass ,class (,@superclasses source)
         ())

       (defmethod create-handle ((,name ,class))
         ,(fun 'create-_))

       (defmethod destroy-handle ((,name ,class) handle)
         (lambda () ,(fun 'destroy-_ 'handle)))
       
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

(define-internal-source wav-source wav)

(defmethod load ((source wav-source) file)
  (cl-soloud-cffi:load-wav
   (handle source) (uiop:native-namestring file)))

(defmethod load-mem ((source wav-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-wav-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-internal-source wav-stream-source wav-stream)

(defmethod load ((source wav-stream-source) file)
  (cl-soloud-cffi:load-wav-stream
   (handle source) (uiop:native-namestring file)))

(defmethod load-mem ((source wav-stream-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-wav-stream-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-internal-source speech-source speech)

(defmethod load ((source speech-source) text)
  (cl-soloud-cffi:set-speech-text
   (handle source) text))

(define-internal-source sfxr-source sfxr)

(defmethod load ((source sfxr-source) file)
  (cl-soloud-cffi:load-sfxr-params
   (handle source) (uiop:native-namestring file)))

(defmethod load ((source sfxr-source) (preset symbol))
  (cl-soloud-cffi:load-sfxr-preset
   (handle source) preset (random (ash 1 (* 8 (cffi:foreign-type-size :int))))))

(defmethod load-mem ((source sfxr-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-sfxr-params-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-internal-source monotone-source monotone)

(defmethod load ((source monotone-source) file)
  (cl-soloud-cffi:load-monotone
   (handle source) (uiop:native-namestring file)))

(defmethod load-mem ((source monotone-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-monotone-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-internal-source ted-sid-source ted-sid)

(defmethod load ((source ted-sid-source) file)
  (cl-soloud-cffi:load-ted-sid
   (handle source) (uiop:native-namestring file)))

(defmethod load-mem ((source ted-sid-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-ted-sid-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-internal-source virtual-audio-source virtual-audio)

(defmacro define-source (name direct-superclasses direct-slots &body options)
  `(defclass ,name (,@direct-superclasses virtual-audio-source)
     ,direct-slots ,@options))

(defgeneric get-audio (audio-source buffer samples))
(defgeneric has-ended (audio-source))
(defgeneric seek-to (audio-source time scratch size))
(defgeneric rewind (audio-source))
(defgeneric get-info (audio-source info-key))

(cffi:defcallback audio-source-get-audio :void ((instance :pointer) (buffer :pointer) (samples :uint))
  (with-callback-handling (instance)
    (get-audio instance buffer samples)))

(cl-soloud-cffi:set-virtual-audio-source-get-audio (cffi:callback audio-source-get-audio))

(cffi:defcallback audio-source-has-ended :void ((instance :pointer))
  (with-callback-handling (instance 1)
    (if (has-ended instance) 1 0)))

(cl-soloud-cffi:set-virtual-audio-source-has-ended (cffi:callback audio-source-has-ended))

(cffi:defcallback audio-source-seek :void ((instance :pointer) (time :float) (scratch :pointer) (size :uint))
  (with-callback-handling (instance)
    (seek-to instance time scratch size)))

(cl-soloud-cffi:set-virtual-audio-source-seek (cffi:callback audio-source-seek))

(cffi:defcallback audio-source-rewind :void ((instance :pointer))
  (with-callback-handling (instance)
    (rewind instance)))

(cl-soloud-cffi:set-virtual-audio-source-rewind (cffi:callback audio-source-rewind))

(cffi:defcallback audio-source-get-info :void ((instance :pointer) (info-key :uint))
  (with-callback-handling (instance 0.0)
    (get-info instance info-key)))

(cl-soloud-cffi:set-virtual-audio-source-get-info (cffi:callback audio-source-get-info))

(defclass virtual-audio-collider (audio-collider)
  ())

(defmethod create-handle ((virtual-audio-collider virtual-audio-collider))
  (cl-soloud-cffi:create-virtual-audio-collider))

(defmethod destroy-handle ((virtual-audio-collider virtual-audio-collider) handle)
  (lambda () (cl-soloud-cffi:destroy-virtual-audio-collider handle)))

(defgeneric collide (collider soloud 3d-data user-data))

(cffi:defcallback audio-collider-collide :void ((instance :pointer) (soloud :pointer) (3d-data :pointer) (user-data :int))
  (with-callback-handling (instance)
    (collide instance (pointer->object soloud) 3d-data user-data)))

(cl-soloud-cffi:set-virtual-audio-collider-collide (cffi:callback audio-collider-collide))

(defclass virtual-audio-attenuator (audio-attenuator)
  ())

(defmethod create-handle ((virtual-audio-attenuator virtual-audio-attenuator))
  (cl-soloud-cffi:create-virtual-audio-attenuator))

(defmethod destroy-handle ((virtual-audio-attenuator virtual-audio-attenuator) handle)
  (lambda () (cl-soloud-cffi:destroy-virtual-audio-attenuator handle)))

(defgeneric attenuate (attenuator distance min-distance max-distance rolloff-factor))

(cffi:defcallback audio-attenuator-attenuate :void ((instance :pointer) (distance :float) (min-distance :float) (max-distance :float) (rolloff-factor :float))
  (with-callback-handling (instance)
    (attenuate instance distance min-distance max-distance rolloff-factor)))

(cl-soloud-cffi:set-virtual-audio-attenuator-attenuate (cffi:callback audio-attenuator-attenuate))

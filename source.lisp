#|
 This file is a part of cl-soloud
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.soloud)

(defclass source (c-backed-object)
  ((filter-map :initform (make-hash-table :test 'eql) :accessor filter-map)))

(defmethod add ((filter filter) (source source))
  (let ((taken (alexandria:hash-table-keys (filter-map source)))
        (all (loop for i from 0 below #.cl-soloud-cffi:*max-filters* collect i)))
    (let ((id (first (set-difference all taken))))
      (assert (not (null id)) (id) "You cannot add more than ~s filters to a source."
              cl-soloud-cffi:*max-filters*)
      (setf (filter source id) filter)))
  filter)

(defmethod withdraw ((filter filter) (source source))
  (loop for k being the hash-keys of (filter-map source)
        for v being the hash-values of (filter-map source)
        do (when (eql v filter) (setf (filter source k) NIL))))

(defclass collider (c-backed-object)
  ())

(defclass attenuator (c-backed-object)
  ())

(defmacro define-internal-source (class direct-superclasses direct-slots &rest options)
  (destructuring-bind (class &optional (name class))
      (alexandria:ensure-list class)
    (flet ((fun (symb &rest args)
             (list* (find-cffi-symbol symb name)
                    `(handle ,name)
                    args)))
      `(progn
         (defclass ,class (,@direct-superclasses source)
           ,direct-slots ,@options)

         (defmethod create-handle ((,name ,class))
           (,(find-cffi-symbol 'create-_ name)))

         (defmethod destroy-handle ((,name ,class) handle)
           (lambda () (,(find-cffi-symbol 'destroy-_ name) handle)))
         
         (defmethod (setf volume) (value (,name ,class) &key fade)
           ,(fun 'set-_-volume 'value)
           value)
         
         (defmethod (setf looping) (value (,name ,class))
           ,(fun 'set-_-looping '(if value 1 0))
           value)
         
         (defmethod (setf min-max-distance) (value (,name ,class))
           (destructuring-bind (min max) value
             ,(fun 'set-_-3d-min-max-distance 'min 'max))
           value)
         
         (defmethod (setf attenuation) (value (,name ,class))
           (destructuring-bind (model rolloff) value
             ,(fun 'set-_-3d-attenuation 'model 'rolloff))
           value)
         
         (defmethod (setf doppler-factor) (value (,name ,class))
           ,(fun 'set-_-3d-doppler-factor 'value)
           value)
         
         (defmethod (setf 3d-processing) (value (,name ,class))
           ,(fun 'set-_-3d-processing '(if value 1 0))
           value)
         
         (defmethod (setf listener-relative) (value (,name ,class))
           ,(fun 'set-_-3d-listener-relative '(if value 1 0))
           value)
         
         (defmethod (setf distance-delay) (value (,name ,class))
           ,(fun 'set-_-3d-distance-delay '(if value 1 0))
           value)
         
         (defmethod (setf collider) (value (,name ,class))
           (if (listp value)
               (destructuring-bind (collider user-data) value
                 ,(fun 'set-_-3d-collider* '(handle collider) 'user-data))
               ,(fun 'set-_-3d-collider '(handle value)))
           value)
         
         (defmethod (setf attenuator) (value (,name ,class))
           ,(fun 'set-_-3d-attenuator '(handle value))
           value)
         
         (defmethod (setf inaudible-behavior) (value (,name ,class))
           (destructuring-bind (must-tick kill) value
             ,(fun 'set-_-3d-inaudible-behavior '(if must-tick 1 0) '(if kill 1 0)))
           value)
         
         (defmethod (setf filter) ((filter filter) (,name ,class) id)
           (check-type id (integer 0 #.cl-soloud-cffi:*max-filters*))
           ,(fun 'set-_-filter 'id '(handle filter))
           value)

         (defmethod (setf filter) ((null null) (,name ,class) id)
           (check-type id (integer 0 #.cl-soloud-cffi:*max-filters*))
           ,(fun 'set-_-filter 'id '(cffi:null-pointer))
           value)
         
         (defmethod stop ((,name ,class))
           ,(fun 'stop-_))))))

(trivial-indent:define-indentation define-internal-source (6 4 &rest 2))

(define-internal-source (wav-source wav) ()
  ())

(defmethod load-file ((source wav-source) file)
  (cl-soloud-cffi:load-wav
   (handle source) (uiop:native-namestring file)))

(defmethod load-mem ((source wav-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-wav-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-internal-source (wav-stream-source wav-stream) ()
  ())

(defmethod load-file ((source wav-stream-source) file)
  (cl-soloud-cffi:load-wav-stream
   (handle source) (uiop:native-namestring file)))

(defmethod load-mem ((source wav-stream-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-wav-stream-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-internal-source (speech-source speech) ()
  ())

(defmethod load-text ((source speech-source) text)
  (cl-soloud-cffi:set-speech-text
   (handle source) text))

(define-internal-source (sfxr-source sfxr) ()
  ())

(defmethod load-file ((source sfxr-source) file)
  (cl-soloud-cffi:load-sfxr-params
   (handle source) (uiop:native-namestring file)))

(defmethod load-preset ((source sfxr-source) (preset symbol))
  (cl-soloud-cffi:load-sfxr-preset
   (handle source) preset (random (ash 1 (* 8 (cffi:foreign-type-size :int))))))

(defmethod load-mem ((source sfxr-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-sfxr-params-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-internal-source (monotone-source monotone) ()
  ())

(defmethod load-file ((source monotone-source) file)
  (cl-soloud-cffi:load-monotone
   (handle source) (uiop:native-namestring file)))

(defmethod load-mem ((source monotone-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-monotone-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-internal-source (ted-sid-source ted-sid) ()
  ())

(defmethod load-file ((source ted-sid-source) file)
  (cl-soloud-cffi:load-ted-sid
   (handle source) (uiop:native-namestring file)))

(defmethod load-mem ((source ted-sid-source) pointer length &key copy take-ownership)
  (cl-soloud-cffi:load-ted-sid-mem*
   (handle source) pointer length (if copy 1 0) (if take-ownership 1 0)))

(define-internal-source (virtual-source virtual-audio-source) ()
  ())

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

(defclass virtual-collider (collider)
  ())

(defmethod create-handle ((virtual-collider virtual-collider))
  (cl-soloud-cffi:create-virtual-audio-collider))

(defmethod destroy-handle ((virtual-collider virtual-collider) handle)
  (lambda () (cl-soloud-cffi:destroy-virtual-audio-collider handle)))

(defgeneric collide (collider soloud 3d-data user-data))

(cffi:defcallback audio-collider-collide :void ((instance :pointer) (soloud :pointer) (3d-data :pointer) (user-data :int))
  (with-callback-handling (instance)
    (collide instance (pointer->object soloud) 3d-data user-data)))

(cl-soloud-cffi:set-virtual-audio-collider-collide (cffi:callback audio-collider-collide))

(defclass virtual-attenuator (attenuator)
  ())

(defmethod create-handle ((virtual-attenuator virtual-attenuator))
  (cl-soloud-cffi:create-virtual-audio-attenuator))

(defmethod destroy-handle ((virtual-attenuator virtual-attenuator) handle)
  (lambda () (cl-soloud-cffi:destroy-virtual-audio-attenuator handle)))

(defgeneric attenuate (attenuator distance min-distance max-distance rolloff-factor))

(cffi:defcallback audio-attenuator-attenuate :void ((instance :pointer) (distance :float) (min-distance :float) (max-distance :float) (rolloff-factor :float))
  (with-callback-handling (instance)
    (attenuate instance distance min-distance max-distance rolloff-factor)))

(cl-soloud-cffi:set-virtual-audio-attenuator-attenuate (cffi:callback audio-attenuator-attenuate))

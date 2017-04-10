#|
 This file is a part of cl-soloud
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.soloud)

(define-internal-source bus ()
  ((soloud :initform NIL :accessor soloud)))

;; We do this so that an access to SOLOUD on a BUS automatically
;; traverses up the ancestry chain to the final SOLOUD object,
;; if any. This is important in order to allow defining the bus
;; tree in an arbitrary fashion.
(defmethod soloud ((soloud soloud))
  soloud)

(defmethod soloud :around ((bus bus))
  (soloud (call-next-method)))

(defmethod soloud :around ((playback playback))
  (soloud (call-next-method)))

;; FIXME: I'm not sure this is entirely safe. As far as I can tell,
;;        SoLoud will automatically remove sources on end, so a bus
;;        might get removed due to ending but retain its reference
;;        on the CL side here. I have no idea what the implications
;;        of this might be.
(defmethod play :after ((source bus) (target soloud) &key)
  (setf (soloud source) target))

(defmethod play :after ((source bus) (target bus) &key)
  (setf (soloud source) target))

(defmethod play ((source source) (bus bus) &key (volume 1.0) (pan 0.0) paused delay location velocity)
  (check-type volume (float 0.0 1.0))
  (check-type pan (float -1.0 1.0))
  (let ((paused (if paused 1 0)))
    (make-instance
     'playback
     :soloud bus
     :source source
     :handle (cond ((and delay location)
                    (destructuring-bind (x y z) location
                      (destructuring-bind (&optional (vx 0.0) (vy 0.0) (vz 0.0)) velocity
                        (cl-soloud-cffi:bus-play-3d-clocked*
                         (handle bus) delay (handle source) x y z vx vy vz volume))))
                   (location
                    (destructuring-bind (x y z) location
                      (destructuring-bind (&optional (vx 0.0) (vy 0.0) (vz 0.0)) velocity
                        (cl-soloud-cffi:bus-play-3d*
                         (handle bus) (handle source) x y z vx vy vz volume paused))))
                   (delay
                    (cl-soloud-cffi:bus-play-clocked* (handle bus) delay (handle source) volume pan))
                   (T
                    (cl-soloud-cffi:bus-play* (handle bus) (handle source) volume pan paused))))))

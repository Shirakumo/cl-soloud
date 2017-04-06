#|
 This file is a part of cl-soloud
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.soloud)

(defgeneric set-parameters (filter &key &allow-other-keys))

(defclass biquad-resonant-filter (filter)
  ())

(defmethod initialize-instance :after ((filter biquad-resonant-filter) &key)
  (let ((handle (cl-soloud-cffi:create-biquad-resonant-filter)))
    (when (<= handle 0)
      (error "Failed to create biquad resonant filter."))
    (setf (handle filter) handle)
    (tg:finalize filter (lambda () (cl-soloud-cffi:destroy-biquad-resonant-filter handle)))))

(defmethod set-parameters ((filter biquad-resonant-filter) &key type sample-rate frequency resonance)
  (cl-soloud-cffi:set-biquad-resonant-filter-params
   (handle filter) type sample-rate frequency resonance))

(defclass echo-filter (filter)
  ())

(defmethod initialize-instance :after ((filter echo-filter) &key)
  (let ((handle (cl-soloud-cffi:create-echo-filter)))
    (when (<= handle 0)
      (error "Failed to create biquad resonant filter."))
    (setf (handle filter) handle)
    (tg:finalize filter (lambda () (cl-soloud-cffi:destroy-echo-filter handle)))))

(defmethod set-parameters ((filter echo-filter) &key delay decay filter)
  (cl-soloud-cffi:set-echo-filter-params*
   (handle filter) delay decay filter))

(defclass lofi-filter (filter)
  ())

(defmethod initialize-instance :after ((filter lofi-filter) &key)
  (let ((handle (cl-soloud-cffi:create-lofi-filter)))
    (when (<= handle 0)
      (error "Failed to create biquad resonant filter."))
    (setf (handle filter) handle)
    (tg:finalize filter (lambda () (cl-soloud-cffi:destroy-lofi-filter handle)))))

(defmethod set-parameters ((filter lofi-filter) &key sample-rate bit-depth)
  (cl-soloud-cffi:set-lofi-filter-params
   (handle filter) sample-rate bit-depth))

(defclass flanger-filter (filter)
  ())

(defmethod initialize-instance :after ((filter flanger-filter) &key)
  (let ((handle (cl-soloud-cffi:create-flanger-filter)))
    (when (<= handle 0)
      (error "Failed to create biquad resonant filter."))
    (setf (handle filter) handle)
    (tg:finalize filter (lambda () (cl-soloud-cffi:destroy-flanger-filter handle)))))

(defmethod set-parameters ((filter flanger-filter) &key delay freq)
  (cl-soloud-cffi:set-flanger-filter-params
   (handle filter) delay freq))

(defclass dc-removal-filter (filter)
  ())

(defmethod initialize-instance :after ((filter dc-removal-filter) &key)
  (let ((handle (cl-soloud-cffi:create-dc-removal-filter)))
    (when (<= handle 0)
      (error "Failed to create biquad resonant filter."))
    (setf (handle filter) handle)
    (tg:finalize filter (lambda () (cl-soloud-cffi:destroy-dc-removal-filter handle)))))

(defmethod set-parameters ((filter dc-removal-filter) &key length)
  (cl-soloud-cffi:set-dc-removal-filter-params*
   (handle filter) length))

(defclass fft-filter (filter)
  ())

(defmethod initialize-instance :after ((filter fft-filter) &key)
  (let ((handle (cl-soloud-cffi:create-fft-filter)))
    (when (<= handle 0)
      (error "Failed to create biquad resonant filter."))
    (setf (handle filter) handle)
    (tg:finalize filter (lambda () (cl-soloud-cffi:destroy-fft-filter handle)))))

(defclass bass-boost-filter (filter)
  ())

(defmethod initialize-instance :after ((filter bass-boost-filter) &key)
  (let ((handle (cl-soloud-cffi:create-bass-boost-filter)))
    (when (<= handle 0)
      (error "Failed to create biquad resonant filter."))
    (setf (handle filter) handle)
    (tg:finalize filter (lambda () (cl-soloud-cffi:destroy-bass-boost-filter handle)))))

(defmethod set-parameters ((filter bass-boost-filter) &key boost)
  (cl-soloud-cffi:set-bass-boost-filter-params
   (handle filter) boost))

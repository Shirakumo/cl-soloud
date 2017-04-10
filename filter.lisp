#|
 This file is a part of cl-soloud
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.soloud)

(defclass filter (c-backed-object)
  ())

(defgeneric set-parameters (filter &key &allow-other-keys))

(defclass biquad-resonant-filter (filter)
  ())

(defmethod create-handle ((biquad-resonant-filter biquad-resonant-filter))
  (cl-soloud-cffi:create-biquad-resonant-filter))

(defmethod destroy-handle ((biquad-resonant-filter biquad-resonant-filter) handle)
  (lambda () (cl-soloud-cffi:destroy-biquad-resonant-filter handle)))

(defmethod set-parameters ((filter biquad-resonant-filter) &key type sample-rate frequency resonance)
  (cl-soloud-cffi:set-biquad-resonant-filter-params
   (handle filter) type sample-rate frequency resonance))

(defclass echo-filter (filter)
  ())

(defmethod create-handle ((echo-filter echo-filter))
  (cl-soloud-cffi:create-echo-filter))

(defmethod destroy-handle ((echo-filter echo-filter) handle)
  (lambda () (cl-soloud-cffi:destroy-echo-filter handle)))

(defmethod set-parameters ((filter echo-filter) &key delay decay filter-type)
  (cl-soloud-cffi:set-echo-filter-params*
   (handle filter) delay decay filter-type))

(defclass lofi-filter (filter)
  ())

(defmethod create-handle ((lofi-filter lofi-filter))
  (cl-soloud-cffi:create-lofi-filter))

(defmethod destroy-handle ((lofi-filter lofi-filter) handle)
  (lambda () (cl-soloud-cffi:destroy-lofi-filter handle)))

(defmethod set-parameters ((filter lofi-filter) &key sample-rate bit-depth)
  (cl-soloud-cffi:set-lofi-filter-params
   (handle filter) sample-rate bit-depth))

(defclass flanger-filter (filter)
  ())

(defmethod create-handle ((flanger-filter flanger-filter))
  (cl-soloud-cffi:create-flanger-filter))

(defmethod destroy-handle ((flanger-filter flanger-filter) handle)
  (lambda () (cl-soloud-cffi:destroy-flanger-filter handle)))

(defmethod set-parameters ((filter flanger-filter) &key delay freq)
  (cl-soloud-cffi:set-flanger-filter-params
   (handle filter) delay freq))

(defclass dc-removal-filter (filter)
  ())

(defmethod create-handle ((dc-removal-filter dc-removal-filter))
  (cl-soloud-cffi:create-dc-removal-filter))

(defmethod destroy-handle ((dc-removal-filter dc-removal-filter) handle)
  (lambda () (cl-soloud-cffi:destroy-dc-removal-filter handle)))

(defmethod set-parameters ((filter dc-removal-filter) &key length)
  (cl-soloud-cffi:set-dc-removal-filter-params*
   (handle filter) length))

(defclass fft-filter (filter)
  ())

(defmethod create-handle ((fft-filter fft-filter))
  (cl-soloud-cffi:create-fft-filter))

(defmethod destroy-handle ((fft-filter fft-filter) handle)
  (lambda () (cl-soloud-cffi:destroy-fft-filter handle)))

(defclass bass-boost-filter (filter)
  ())

(defmethod create-handle ((bass-boost-filter bass-boost-filter))
  (cl-soloud-cffi:create-bass-boost-filter))

(defmethod destroy-handle ((bass-boost-filter bass-boost-filter) handle)
  (lambda () (cl-soloud-cffi:destroy-bass-boost-filter handle)))

(defmethod set-parameters ((filter bass-boost-filter) &key boost)
  (cl-soloud-cffi:set-bass-boost-filter-params
   (handle filter) boost))

(defclass virtual-filter (filter)
  ())

(defmethod create-handle ((virtual-filter virtual-filter))
  (cl-soloud-cffi:create-virtual-filter))

(defmethod destroy-handle ((virtual-filter virtual-filter) handle)
  (lambda () (cl-soloud-cffi:destroy-virtual-filter handle)))

(defgeneric filter (filter samples channels samplerate time))
(defgeneric filter-channel (filter buffer channel samples channels samplerate time))

(cffi:defcallback filter-filter :void ((instance :pointer) (samples :uint) (channels :uint) (samplerate :float) (time :float))
  (with-callback-handling (instance)
    (filter instance samples channels samplerate time)))

(cl-soloud-cffi:set-virtual-filter-filter (cffi:callback filter-filter))

(cffi:defcallback filter-filter-channel :void ((instance :pointer) (buffer :pointer) (samples :uint) (samplerate :float) (time :float) (channel :uint) (channels :uint))
  (with-callback-handling (instance)
    (filter-channel instance buffer channel samples channels samplerate time)))

(cl-soloud-cffi:set-virtual-filter-filter-channel (cffi:callback filter-filter-channel))

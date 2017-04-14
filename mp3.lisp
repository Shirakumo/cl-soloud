#|
 This file is a part of cl-soloud
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.soloud)

(defclass mp3-source (virtual-source)
  ((file :initform NIL :accessor file)))

(defmethod load-file ((source mp3-source) file)
  (let ((file (cl-mpg123:make-file
               file
               :buffer-size NIL
               :accepted-format (list (round (base-samplerate source)) :stereo :float))))
    (cl-mpg123:connect file)
    (cl-mpg123:scan file)
    (setf (file source) file)))

(defmethod get-audio ((source mp3-source) buffer samples)
  (let ((read (cl-mpg123:read-directly (file source) buffer samples)))
    ;; make sure to zero memory if we reached the end.
    (loop for i from read below samples
          do (setf (cffi:mem-aref buffer :float i) 0.0s0))))

(defmethod has-ended ((source mp3-source))
  (<= (cl-mpg123:sample-count (file source))
      (cl-mpg123:sample-position (file source))))

(defmethod seek-to ((source mp3-source) time scratch size)
  ;; I don't actually know what these arguments are.
  )

(defmethod rewind ((source mp3-source))
  (cl-mpg123:seek (file source) 0)
  0)

(defmethod get-info ((source mp3-source) info-key)
  0.0)

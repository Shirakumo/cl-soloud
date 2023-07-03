(in-package #:org.shirakumo.fraf.soloud)

(defclass soloud (c-backed-object)
  ())

(defclass playback ()
  ((handle :initarg :handle)
   (soloud :initarg :soloud :initform NIL :accessor soloud)
   (source :initarg :source :initform NIL :accessor source)))

(defun compute-soloud-flags (flags)
  (etypecase flags
    (integer flags)
    (list (let ((int 0))
            (dolist (flag flags int)
              (setf int (logior int (cffi:foreign-enum-value
                                     'cl-soloud-cffi:soloud-flag flag))))))))

(defmethod create-handle ((soloud soloud))
  (cl-soloud-cffi:create))

(defmethod destroy-handle ((soloud soloud) handle)
  (lambda ()
    (cl-soloud-cffi:deinit handle)
    (cl-soloud-cffi:destroy handle)))

(defmethod initialize-instance :after ((soloud soloud) &key (flags '(:clip-roundoff)) (backend :auto) sample-rate buffer-size channels
                                                            (max-active-playback-count 16))
  ;; FIXME: detect init fail
  (cl-soloud-cffi:init* (handle soloud) (compute-soloud-flags flags) backend (or sample-rate 0) (or buffer-size 0) (or channels 2))
  (cl-soloud-cffi:set-max-active-voice-count (handle soloud) max-active-playback-count))

(defmethod backend ((soloud soloud))
  (values
   (cl-soloud-cffi:get-backend-id (handle soloud))
   (cl-soloud-cffi:get-backend-string (handle soloud))
   (cl-soloud-cffi:get-backend-channels (handle soloud))
   (cl-soloud-cffi:get-backend-sample-rate (handle soloud))
   (cl-soloud-cffi:get-backend-buffer-size (handle soloud))))

(defgeneric channel-location (soloud channel))

(defmethod (setf channel-location) (location (soloud soloud) channel)
  (destructuring-bind (x y z) location
    (check-type channel (integer 0))
    (cl-soloud-cffi:set-speaker-position (handle soloud) channel x y z)))

(defgeneric play (source target &key volume pan paused delay location velocity &allow-other-keys))

(defmethod play ((source source) (soloud soloud) &key (volume 1.0) (pan 0.0) paused delay location velocity (bus 0))
  (check-type volume (float 0.0 1.0))
  (check-type pan (float -1.0 1.0))
  (when (<= cl-soloud-cffi:*max-sources* (playback-count soloud))
    (error "Cannot play source: reached maximum number of total playbacks (~d)"
           cl-soloud-cffi:*max-sources*))
  (let ((paused (if paused 1 0)))
    (make-instance
     'playback
     :soloud soloud
     :source source
     :handle (cond ((and delay location)
                    (destructuring-bind (x y z) location
                      (destructuring-bind (&optional (vx 0.0) (vy 0.0) (vz 0.0)) velocity
                        (cl-soloud-cffi:play-3d-clocked*
                         (handle soloud) delay (handle source) x y z vx vy vz volume paused bus))))
                   (location
                    (destructuring-bind (x y z) location
                      (destructuring-bind (&optional (vx 0.0) (vy 0.0) (vz 0.0)) velocity
                        (cl-soloud-cffi:play-3d*
                         (handle soloud) (handle source) x y z vx vy vz volume paused bus))))
                   (delay
                    (cl-soloud-cffi:play-clocked* (handle soloud) delay (handle source) volume pan paused bus))
                   (T
                    (cl-soloud-cffi:play* (handle soloud) (handle source) volume pan paused bus))))))

(defmethod seek ((playback playback) seconds)
  (cl-soloud-cffi:seek (handle (soloud playback)) (handle playback) seconds))

(defmethod volume ((soloud soloud))
  (cl-soloud-cffi:get-global-volume (handle soloud)))

(defmethod (setf volume) (volume (soloud soloud) &key fade)
  (check-type volume (float 0.0 1.0))
  (check-type fade (or null (float 0.0)))
  (if fade
      (cl-soloud-cffi:fade-global-volume (handle soloud) volume fade)
      (cl-soloud-cffi:set-global-volume (handle soloud) volume)))

(defmethod volume ((playback playback))
  (cl-soloud-cffi:get-volume (handle (soloud playback)) (handle playback)))

(defmethod (setf volume) (volume (playback playback) &key fade)
  (check-type volume (float 0.0 1.0))
  (check-type fade (or null (float 0.0)))
  (if fade
      (cl-soloud-cffi:fade-volume (handle (soloud playback)) (handle playback) volume fade)
      (cl-soloud-cffi:set-volume (handle (soloud playback)) (handle playback) volume)))

(defmethod pan ((playback playback))
  (cl-soloud-cffi:get-pan (handle (soloud playback)) (handle playback)))

(defmethod (setf pan) (pan (playback playback) &key fade)
  (check-type pan (float -1.0 1.0))
  (check-type fade (or null (float 0.0)))
  (if fade
      (cl-soloud-cffi:fade-pan (handle (soloud playback)) (handle playback) pan fade)
      (cl-soloud-cffi:set-pan (handle (soloud playback)) (handle playback) pan)))

(defmethod set-absolute-pan ((playback playback) &key front-left front-right
                                                      (back-left front-left)
                                                      (back-right front-right)
                                                      (center (/ (+ front-left front-right) 2))
                                                      (subwoofer 1.0))
  (check-type front-left (float 0.0 1.0))
  (check-type front-right (float 0.0 1.0))
  (check-type back-left (float 0.0 1.0))
  (check-type back-right (float 0.0 1.0))
  (check-type center (float 0.0 1.0))
  (check-type subwoofer (float 0.0 1.0))
  (cl-soloud-cffi:set-pan-absolute* (handle (soloud playback)) (handle playback)
                                    front-left front-right back-left back-right
                                    center subwoofer))

(defmethod relative-speed ((playback playback))
  (cl-soloud-cffi:get-relative-play-speed (handle (soloud playback)) (handle playback)))

(defmethod (setf relative-speed) (relative-speed (playback playback) &key fade)
  (check-type relative-speed float)
  (check-type fade (or null (float 0.0)))
  (if fade
      (cl-soloud-cffi:fade-relative-play-speed (handle (soloud playback)) (handle playback) relative-speed fade)
      (cl-soloud-cffi:set-relative-play-speed (handle (soloud playback)) (handle playback) relative-speed)))

(defmethod paused-p ((playback playback))
  (if (= 0 (cl-soloud-cffi:get-pause (handle (soloud playback)) (handle playback)))
      NIL
      T))

(defmethod (setf paused-p) (value (playback playback))
  (etypecase value
    ((float 0.0)
     (cl-soloud-cffi:schedule-pause (handle (soloud playback)) (handle playback) value))
    ((eql T)
     (cl-soloud-cffi:set-pause (handle (soloud playback)) (handle playback) 1))
    ((eql NIL)
     (cl-soloud-cffi:set-pause (handle (soloud playback)) (handle playback) 0))))

(defmethod stop ((playback playback))
  (cl-soloud-cffi:stop (handle (soloud playback)) (handle playback)))

(defmethod stop ((soloud soloud))
  (cl-soloud-cffi:stop-all (handle soloud)))

(defmethod looping-p ((playback playback))
  (if (= 0 (cl-soloud-cffi:get-looping (handle (soloud playback)) (handle playback)))
      NIL
      T))

(defmethod (setf looping-p) (value (playback playback))
  (cl-soloud-cffi:set-looping (handle (soloud playback)) (handle playback)
                            (if value 1 0)))

(defmethod protected-p ((playback playback))
  (if (= 0 (cl-soloud-cffi:get-protect-voice (handle (soloud playback)) (handle playback)))
      NIL
      T))

(defmethod (setf protected-p) (value (playback playback))
  (cl-soloud-cffi:set-protect-voice (handle (soloud playback)) (handle playback)
                            (if value 1 0)))

(defmethod max-active-playback-count ((soloud soloud))
  (cl-soloud-cffi:get-max-active-voice-count (handle soloud)))

(defmethod (setf max-active-playback-count) (count (soloud soloud))
  (check-type count (integer 0))
  (cl-soloud-cffi:set-max-active-voice-count (handle soloud) count))

(defmethod active-playback-count ((soloud soloud))
  (cl-soloud-cffi:get-active-voice-count (handle soloud)))

(defmethod playback-count ((soloud soloud))
  (cl-soloud-cffi:get-voice-count (handle soloud)))

(defmethod sample-rate ((playback playback))
  (cl-soloud-cffi:get-sample-rate (handle (soloud playback)) (handle playback)))

(defmethod (setf sample-rate) (sample-rate (playback playback))
  (check-type sample-rate (integer 0))
  (cl-soloud-cffi:set-sample-rate (handle (soloud playback)) (handle playback) sample-rate))

(defmethod oscillate-volume ((soloud soloud) from to time)
  (check-type from (float 0.0 1.0))
  (check-type to (float 0.0 1.0))
  (check-type time (float 0.0))
  (cl-soloud-cffi:oscillate-global-volume (handle soloud) from to time))

(defmethod oscillate-volume ((playback playback) from to time)
  (check-type from (float 0.0 1.0))
  (check-type to (float 0.0 1.0))
  (check-type time (float 0.0))
  (cl-soloud-cffi:oscillate-volume (handle (soloud playback)) (handle playback) from to time))

(defmethod oscillate-pan ((playback playback) from to time)
  (check-type from (float -1.0 1.0))
  (check-type to (float -1.0 1.0))
  (check-type time (float 0.0))
  (cl-soloud-cffi:oscillate-pan (handle (soloud playback)) (handle playback) from to time))

(defmethod oscillate-relative-speed ((playback playback) from to time)
  (check-type time (float 0.0))
  (cl-soloud-cffi:oscillate-relative-play-speed (handle (soloud playback)) (handle playback) from to time))

(defmethod sound-speed ((soloud soloud))
  (cl-soloud-cffi:get-3d-sound-speed (handle soloud)))

(defmethod (setf sound-speed) (value (soloud soloud))
  (check-type value (float 0.0))
  (cl-soloud-cffi:set-3d-sound-speed (handle soloud) speed)
  (cl-soloud-cffi:update-3d-audio (handle soloud)))

(defmethod (setf location) (value (soloud soloud))
  (destructuring-bind (x y z) value
    (cl-soloud-cffi:set-3d-listener-position (handle soloud) x y z))
  (cl-soloud-cffi:update-3d-audio (handle soloud)))

(defgeneric direction (object))

(defmethod (setf direction) (value (soloud soloud))
  (destructuring-bind (x y z) value
    (cl-soloud-cffi:set-3d-listener-at (handle soloud) x y z))
  (cl-soloud-cffi:update-3d-audio (handle soloud)))

(defgeneric up (object))

(defmethod (setf up) (value (soloud soloud))
  (destructuring-bind (x y z) value
    (cl-soloud-cffi:set-3d-listener-up (handle soloud) x y z))
  (cl-soloud-cffi:update-3d-audio (handle soloud)))

(defmethod (setf velocity) (value (soloud soloud))
  (destructuring-bind (x y z) value
    (cl-soloud-cffi:set-3d-listener-velocity (handle soloud) x y z))
  (cl-soloud-cffi:update-3d-audio (handle soloud)))

(defmethod (setf location) (value (playback playback))
  (destructuring-bind (x y z) value
    (cl-soloud-cffi:set-3d-source-position (handle (soloud playback)) (handle playback) x y z))
  (cl-soloud-cffi:update-3d-audio (handle soloud)))

(defmethod (setf velocity) (value (playback playback))
  (destructuring-bind (x y z) value
    (cl-soloud-cffi:set-3d-source-velocity (handle (soloud playback)) (handle playback) x y z))
  (cl-soloud-cffi:update-3d-audio (handle soloud)))

(defmethod (setf min-max-distance) (value (playback playback))
  (destructuring-bind (min max) value
    (cl-soloud-cffi:set-3d-source-min-max-distance (handle (soloud playback)) (handle playback) min max))
  (cl-soloud-cffi:update-3d-audio (handle soloud)))

(defmethod (setf attenuation) (value (playback playback))
  (destructuring-bind (attenuation rolloff) value
    (cl-soloud-cffi:set-3d-source-attenuation (handle (soloud playback)) (handle playback) attenuation roloff))
  (cl-soloud-cffi:update-3d-audio (handle soloud)))

(defmethod (setf doppler-factor) (value (playback playback))
  (cl-soloud-cffi:set-3d-source-doppler-factor (handle (soloud playback)) (handle playback) value)
  (cl-soloud-cffi:update-3d-audio (handle soloud)))

(defmethod filter-parameter ((playback playback) filter attribute)
  (check-type filter (integer 0 #.(1- cl-soloud-cffi:*max-filters*)))
  (cl-soloud-cffi:get-filter-parameter (handle (soloud playback)) (handle playback) filter attribute))

(defmethod (setf filter-parameter) (value (playback playback) filter attribute)
  (check-type filter (integer 0 #.(1- cl-soloud-cffi:*max-filters*)))
  (cl-soloud-cffi:set-filter-parameter (handle (soloud playback)) (handle playback) filter attribute value))

(defmethod (setf inaudible-behavior) (value (playback playback))
  (destructuring-bind (tick-inaudible kill-inaudible) value
    (cl-soloud-cffi:set-inaudible-behavior (handle (soloud playback)) (handle playback)
                                           (if tick-inaudible 1 0) (if kill-inaudible 1 0))))

(defmethod playing ((soloud soloud))
  (< 0 (active-playback-count soloud)))

(defclass group (playback)
  ())

;; FIXME
(defmethod initialize-instance :after ((group group) &key soloud)
  (let ((handle (cl-soloud-cffi:create-voice-group (handle soloud))))
    (when (<= handle 0)
      (error "Failed to create group."))
    (setf (handle group) handle)
    (tg:finalize group (lambda () (cl-soloud-cffi:destroy-voice-group (handle soloud) handle)))))

(defmethod add ((playback playback) (group group))
  (unless (eql (soloud playback) (soloud group))
    (error "The soloud backend of the playback and group do not match."))
  (cl-soloud-cffi:add-voice-to-group (handle (soloud group)) (handle group) (handle playback)))

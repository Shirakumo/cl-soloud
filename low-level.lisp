#|
 This file is a part of cl-soloud
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.soloud.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(defvar *max-filters* 8)
(defvar *max-sources* 1024)

(define-foreign-library libsoloud
  (:darwin (:or "libsoloud.dylib" "libsoloud.so"
                #+X86 "mac32-libsoloud.dylib"
                #+X86-64 "mac64-libsoloud.dylib"))
  (:unix (:or "libsoloud.so"
              #+X86 "lin32-libsoloud.so"
              #+X86-64 "lin64-libsoloud.so"))
  (:windows (:or "out123.dll"
                 #+X86 "win32-libsoloud.dll"
                 #+X86-64 "win64-libsoloud.dll"))
  (t (:default "soloud")))

(use-foreign-library libsoloud)

;;; SoLoud
(defctype soloud :pointer)
(defctype audio-collider :pointer)
(defctype audio-attenuator :pointer)
(defctype audio-source :pointer)
(defctype file :pointer)
(defctype filter :pointer)

(defcenum soloud-error
  (:no-error              0)
  (:invalid-parameter     1)
  (:file-not-found        2)
  (:file-load-failed      3)
  (:dll-not-found         4)
  (:out-of-memory         5)
  (:not-implemented       6)
  (:unknown-error         7))

(defcenum audio-backend
  (:auto                  0)
  (:sdl                   1)
  (:sdl2                  2)
  (:portaudio             3)
  (:winmm                 4)
  (:xaudio2               5)
  (:wasapi                6)
  (:alsa                  7)
  (:oss                   8)
  (:openal                9)
  (:coreaudio            10)
  (:opensles             11)
  (:nulldriver           12)
  (:backend-max          13))

(defcenum soloud-flag
  (:clip-roundoff         1)
  (:enable-visualization  2)
  (:left-handed-3d        4))

(defcenum attenuation-model
  (:no-attenuation        0)
  (:inverse-distance      1)
  (:linear-distance       2)
  (:exponential-distance  3))

(defcenum audio-source-flag
  (:should-loop           1)
  (:single-instance       2)
  (:visualization-data    4)
  (:process-3d            8)
  (:listener-relative    16)
  (:distance-delay       32)
  (:inaudible-kill       64)
  (:inaudible-tick      128))

(defcfun (create "Soloud_create") soloud)

(defcfun (destroy "Soloud_destroy") :void
  (soloud soloud))

(defcfun (init "Soloud_init") :void
  (soloud soloud))

(defcfun (init* "Soloud_initEx") :void
  (soloud soloud)
  (flags soloud-flag)
  (backend audio-backend)
  (sample-rate :uint)
  (buffer-size :uint)
  (channels :uint))

(defcfun (deinit "Soloud_deinit") :void
  (soloud soloud))

(defcfun (get-version "Soloud_getVersion") :uint
  (soloud soloud))

(defcfun (get-error-string "Soloud_getErrorString") :string
  (soloud soloud)
  (error-code :int))

(defcfun (get-backend-id "Soloud_getBackendId") audio-backend
  (soloud soloud))

(defcfun (get-backend-string "Soloud_getBackendString") :string
  (soloud soloud))

(defcfun (get-backend-channels "Soloud_getBackendChannels") :uint
  (soloud soloud))

(defcfun (get-backend-sample-rate "Soloud_getBackendSamplerate") :uint
  (soloud soloud))

(defcfun (get-backend-buffer-size "Soloud_getBackendBufferSize") :uint
  (soloud soloud))

(defcfun (set-speaker-position "Soloud_setSpeakerPosition") :int
  (soloud soloud)
  (channel :uint)
  (x :float)
  (y :float)
  (z :float))

(defcfun (play "Soloud_play") :uint
  (soloud soloud)
  (sound audio-source))

(defcfun (play* "Soloud_playEx") :uint
  (soloud soloud)
  (sound audio-source)
  (volume :float)
  (pan :float)
  (paused :int)
  (bus :uint))

(defcfun (play-clocked "Soloud_playClocked") :uint
  (soloud soloud)
  (sound-time :double)
  (sound audio-source))

(defcfun (play-clocked* "Soloud_playClockedEx") :uint
  (soloud soloud)
  (sound-time :double)
  (sound audio-source)
  (volume :float)
  (pan :float)
  (paused :int)
  (bus :uint))

(defcfun (play-3d "Soloud_play3d") :uint
  (soloud soloud)
  (sound audio-source)
  (x :float)
  (y :float)
  (z :float))

(defcfun (play-3d* "Soloud_play3dEx") :uint
  (soloud soloud)
  (sound audio-source)
  (x :float)
  (y :float)
  (z :float)
  (vx :float)
  (vy :float)
  (vz :float)
  (volume :float)
  (paused :int)
  (bus :int))

(defcfun (play-3d-clocked "Soloud_play3dClocked") :uint
  (soloud soloud)
  (sound-time :double)
  (sound audio-source)
  (x :float)
  (y :float)
  (z :float))

(defcfun (play-3d-clocked* "Soloud_play3dClockedEx") :uint
  (soloud soloud)
  (sound-time :double)
  (sound audio-source)
  (x :float)
  (y :float)
  (z :float)
  (vx :float)
  (vy :float)
  (vz :float)
  (volume :float)
  (paused :int)
  (bus :int))

(defcfun (seek "Soloud_seek") :void
  (soloud soloud)
  (voice-handle :uint)
  (seconds :double))

(defcfun (stop "Soloud_stop") :void
  (soloud soloud)
  (voice-handle :uint))

(defcfun (stop-all "Soloud_stopAll") :void
  (soloud soloud))

(defcfun (stop-audio-source "Soloud_stopAudioSource") :void
  (soloud soloud)
  (source audio-source))

(defcfun (set-filter-parameter "Soloud_setFilterParameter") :void
  (soloud soloud)
  (voice-handle :uint)
  (filter-id :uint)
  (attribute-id :uint)
  (value :float))

(defcfun (get-filter-parameter "Soloud_getFilterParameter") :float
  (soloud soloud)
  (voice-handle :uint)
  (filter-id :uint)
  (attribute-id :uint))

(defcfun (fade-filter-parameter "Soloud_fadeFilterParameter") :void
  (soloud soloud)
  (voice-handle :uint)
  (filter-id :uint)
  (attribute-id :uint)
  (to :float)
  (time :double))

(defcfun (oscillate-filter-parameter "Soloud_oscillateFilterParameter") :void
  (soloud soloud)
  (voice-handle :uint)
  (filter-id :uint)
  (attribute-id :uint)
  (from :float)
  (to :float)
  (time :double))

(defcfun (get-stream-time "Soloud_getStreamTime") :double
  (soloud soloud)
  (voice-handle :uint))

(defcfun (get-pause "Soloud_getPause") :int
  (soloud soloud)
  (voice-handle :uint))

(defcfun (get-volume "Soloud_getVolume") :float
  (soloud soloud)
  (voice-handle :uint))

(defcfun (get-overall-volume "Soloud_getOverallVolume") :float
  (soloud soloud)
  (voice-handle :uint))

(defcfun (get-pan "Soloud_getPan") :float
  (soloud soloud)
  (voice-handle :uint))

(defcfun (get-sample-rate "Soloud_getSamplerate") :float
  (soloud soloud)
  (voice-handle :uint))

(defcfun (get-protect-voice "Soloud_getProtectVoice") :int
  (soloud soloud)
  (voice-handle :uint))

(defcfun (is-valid-voice-handle "Soloud_isValidVoiceHandle") :int
  (soloud soloud)
  (voice-handle :uint))

(defcfun (get-relative-play-speed "Soloud_getRelativePlaySpeed") :float
  (soloud soloud)
  (voice-handle :uint))

(defcfun (get-post-clip-scaler "Soloud_getPostClipScaler") :float
  (soloud soloud))

(defcfun (get-global-volume "Soloud_getGlobalVolume") :float
  (soloud soloud))

(defcfun (get-max-active-voice-count "Soloud_getMaxActiveVoiceCount") :uint
  (soloud soloud))

(defcfun (get-voice-count "Soloud_getVoiceCount") :uint
  (soloud soloud))

(defcfun (get-active-voice-count "Soloud_getActiveVoiceCount") :uint
  (soloud soloud))

(defcfun (get-looping "Soloud_getLooping") :int
  (soloud soloud)
  (voice-handle :uint))

(defcfun (set-looping "Soloud_setLooping") :void
  (soloud soloud)
  (voice-handle :uint)
  (looping :int))

(defcfun (set-max-active-voice-count "Soloud_setMaxActiveVoiceCount") :int
  (soloud soloud)
  (voice-count :uint))

(defcfun (set-inaudible-behavior "Soloud_setInaudibleBehavior") :void
  (soloud soloud)
  (voice-handle :uint)
  (must-tick :int)
  (kill :int))

(defcfun (set-global-volume "Soloud_setGlobalVolume") :void
  (soloud soloud)
  (volume :float))

(defcfun (set-post-clip-scaler "Soloud_setPostClipScaler") :void
  (soloud soloud)
  (scaler :float))

(defcfun (set-pause "Soloud_setPause") :void
  (soloud soloud)
  (voice-handle :uint)
  (pause :int))

(defcfun (set-pause-all "Soloud_setPauseAll") :void
  (soloud soloud)
  (pause :int))

(defcfun (set-relative-play-speed "Soloud_setRelativePlaySpeed") :int
  (soloud soloud)
  (voice-handle :uint)
  (speed :float))

(defcfun (set-protect-voice "Soloud_setProtectVoice") :void
  (soloud soloud)
  (voice-handle :uint)
  (protect :int))

(defcfun (set-sample-rate "Soloud_setSamplerate") :void
  (soloud soloud)
  (voice-handle :uint)
  (sample-rate :float))

(defcfun (set-pan "Soloud_setPan") :void
  (soloud soloud)
  (voice-handle :uint)
  (pan :float))

(defcfun (set-pan-absolute "Soloud_setPanAbsolute") :void
  (soloud soloud)
  (voice-handle :uint)
  (left-volume :float)
  (right-volume :float))

(defcfun (set-pan-absolute* "Soloud_setPanAbsoluteEx") :void
  (soloud soloud)
  (voice-handle :uint)
  (front-left-volume :float)
  (front-right-volume :float)
  (back-left-volume :float)
  (back-right-volume :float)
  (center-volume :float)
  (subwoofer-volume :float))

(defcfun (set-volume "Soloud_setVolume") :void
  (soloud soloud)
  (voice-handle :uint)
  (volume :float))

(defcfun (set-delay-samples "Soloud_setDelaySamples") :void
  (soloud soloud)
  (voice-handle :uint)
  (samples :uint))

(defcfun (fade-volume "Soloud_fadeVolume") :void
  (soloud soloud)
  (voice-handle :uint)
  (to :float)
  (time :double))

(defcfun (fade-pan "Soloud_fadePan") :void
  (soloud soloud)
  (voice-handle :uint)
  (to :float)
  (time :double))

(defcfun (fade-relative-play-speed "Soloud_fadeRelativePlaySpeed") :void
  (soloud soloud)
  (voice-handle :uint)
  (to :float)
  (time :double))

(defcfun (fade-global-volume "Soloud_fadeGlobalVolume") :void
  (soloud soloud)
  (to :float)
  (time :double))

(defcfun (schedule-pause "Soloud_schedulePause") :void
  (soloud soloud)
  (voice-handle :uint)
  (time :double))

(defcfun (schedule-stop "Soloud_scheduleStop") :void
  (soloud soloud)
  (voice-handle :uint)
  (time :double))

(defcfun (oscillate-volume "Soloud_oscillateVolume") :void
  (soloud soloud)
  (voice-handle :uint)
  (from :float)
  (to :float)
  (time :double))

(defcfun (oscillate-pan "Soloud_oscillatePan") :void
  (soloud soloud)
  (voice-handle :uint)
  (from :float)
  (to :float)
  (time :double))

(defcfun (oscillate-relative-play-speed "Soloud_oscillateRelativePlaySpeed") :void
  (soloud soloud)
  (voice-handle :uint)
  (from :float)
  (to :float)
  (time :double))

(defcfun (oscillate-global-volume "Soloud_oscillateGlobalVolume") :void
  (soloud soloud)
  (from :float)
  (to :float)
  (time :double))

(defcfun (set-global-filter "Soloud_setGlobalFilter") :void
  (soloud soloud)
  (filter-id :uint)
  (filter filter))

(defcfun (set-visualization "Soloud_setVisualizationEnable") :void
  (soloud soloud)
  (enable :int))

(defcfun (calc-fft "Soloud_calcFFT") :pointer
  (soloud soloud))

(defcfun (get-wave "Soloud_getWave") :pointer
  (soloud soloud))

(defcfun (get-loop-count "Soloud_getLoopCount") :uint
  (soloud soloud)
  (voice-handle :uint))

(defcfun (get-info "Soloud_getInfo") :float
  (soloud soloud)
  (voice-handle :uint)
  (info-key :uint))

(defcfun (create-voice-group "Soloud_createVoiceGroup") :uint
  (soloud soloud))

(defcfun (destroy-voice-group "Soloud_destroyVoiceGroup") :int
  (soloud soloud)
  (voice-group-handle :uint))

(defcfun (add-voice-to-group "Soloud_addVoiceToGroup") :int
  (soloud soloud)
  (voice-group-handle :uint)
  (voice-handle :uint))

(defcfun (is-voice-group "Soloud_isVoiceGroup") :int
  (soloud soloud)
  (voice-group-handle :uint))

(defcfun (is-voice-group-empty "Soloud_isVoiceGroupEmpty") :int
  (soloud soloud)
  (voice-group-handle :uint))

(defcfun (update-3d-audio "Soloud_update3dAudio") :void
  (soloud soloud))

(defcfun (set-3d-sound-speed "Soloud_set3dSoundSpeed") :int
  (soloud soloud)
  (speed :float))

(defcfun (get-3d-sound-speed "Soloud_get3dSoundSpeed") :float
  (soloud soloud))

(defcfun (set-3d-listener-parameters "Soloud_set3dListenerParameters") :void
  (soloud soloud)
  (x :float)
  (y :float)
  (z :float)
  (x-at :float)
  (y-at :float)
  (z-at :float)
  (x-up :float)
  (y-up :float)
  (z-up :float))

(defcfun (set-3d-listener-parameters* "Soloud_set3dListenerParametersEx") :void
  (soloud soloud)
  (x :float)
  (y :float)
  (z :float)
  (x-at :float)
  (y-at :float)
  (z-at :float)
  (x-up :float)
  (y-up :float)
  (z-up :float)
  (vx :float)
  (vy :float)
  (vz :float))

(defcfun (set-3d-listener-position "Soloud_set3dListenerPosition") :void
  (soloud soloud)
  (x :float)
  (y :float)
  (z :float))

(defcfun (set-3d-listener-at "Soloud_set3dListenerAt") :void
  (soloud soloud)
  (x :float)
  (y :float)
  (z :float))

(defcfun (set-3d-listener-up "Soloud_set3dListenerUp") :void
  (soloud soloud)
  (x :float)
  (y :float)
  (z :float))

(defcfun (set-3d-listener-velocity "Soloud_set3dListenerVelocity") :void
  (soloud soloud)
  (x :float)
  (y :float)
  (z :float))

(defcfun (set-3d-source-parameters "Soloud_set3dSourceParameters") :void
  (soloud soloud)
  (voice-handle :uint)
  (x :float)
  (y :float)
  (z :float))

(defcfun (set-3d-source-parameters* "Soloud_set3dSourceParametersEx") :void
  (soloud soloud)
  (voice-handle :uint)
  (x :float)
  (y :float)
  (z :float)
  (vx :float)
  (vy :float)
  (vz :float))

(defcfun (set-3d-source-position "Soloud_set3dSourcePosition") :void
  (soloud soloud)
  (voice-handle :uint)
  (x :float)
  (y :float)
  (z :float))

(defcfun (set-3d-source-velocity "Soloud_set3dSourceVelocity") :void
  (soloud soloud)
  (voice-handle :uint)
  (x :float)
  (y :float)
  (z :float))

(defcfun (set-3d-source-min-max-distance "Soloud_set3dSourceMinMaxDistance") :void
  (soloud soloud)
  (voice-handle :uint)
  (min :float)
  (max :float))

(defcfun (set-3d-source-attenuation "Soloud_set3dSourceAttenuation") :void
  (soloud soloud)
  (voice-handle :uint)
  (attenuation-model attenuation-model)
  (attenuation-rolloff-factor :float))

(defcfun (set-3d-source-doppler-factor "Soloud_set3dSourceDopplerFactor") :void
  (soloud soloud)
  (voice-handle :uint)
  (doppler-factor :float))

(defcfun (mix "Soloud_mix") :void
  (soloud soloud)
  (buffer :pointer)
  (samples :uint))

(defcfun (mix-signed-16 "Soloud_mixSigned16") :void
  (soloud soloud)
  (buffer :pointer)
  (samples :uint))

;;; Audio Attenuator
(defcfun (destroy-audio-attenuator "AudioAttenuator_destroy") :void
  (audio-attenuator audio-attenuator))

(defcfun (attenuate-audio-attenuator "AudioAttenuator_attenuate") :void
  (audio-attenuator audio-attenuator)
  (distance :float)
  (min :float)
  (max :float)
  (rolloff :float))

;;; Biquad Resonant Filter
(defcenum biquad-resonant-filter-type
  (:none                  0)
  (:lowpass               1)
  (:highpass              2)
  (:bandpass              3))

(defcenum biquad-resonant-filter-flag
  (:wet                   0)
  (:samplerate            1)
  (:frequency             2)
  (:resonance             3))

(defctype biquad-resonant-filter :pointer)

(defcfun (destroy-biquad-resonant-filter "BiquadResonantFilter_destroy") :void
  (biquad-resonant-filter biquad-resonant-filter))

(defcfun (create-biquad-resonant-filter "BiquadResonantFilter_create") biquad-resonant-filter)

(defcfun (set-biquad-resonant-filter-params "BiquadResonantFilter_setParams") :int
  (biquad-resonant-filter biquad-resonant-filter)
  (type biquad-resonant-filter-type)
  (sample-rate :float)
  (frequency :float)
  (resonance :float))

;;; Lofi Filter
(defcenum lofi-filter-flag
  (:wet                   0)
  (:samplerate            1)
  (:bitdepth              2))

(defctype lofi-filter :pointer)

(defcfun (destroy-lofi-filter "LofiFilter_destroy") :void
  (lofi-filter lofi-filter))

(defcfun (create-lofi-filter "LofiFilter_create") lofi-filter)

(defcfun (set-lofi-filter-params "LofiFilter_setParams") :int
  (lofi-filter lofi-filter)
  (sample-rate :float)
  (bit-depth :float))

;;; Bus
(defctype bus :pointer)

(defcfun (destroy-bus "Bus_destroy") :void
  (bus bus))

(defcfun (create-bus "Bus_create") bus)

(defcfun (set-bus-filter "Bus_setFilter") :void
  (bus bus)
  (filter-id :uint)
  (filter filter))

(defcfun (bus-play "Bus_play") :uint
  (bus bus)
  (sound audio-source))

(defcfun (bus-play* "Bus_playEx") :uint
  (bus bus)
  (sound audio-source)
  (volume :float)
  (pan :float)
  (paused :int))

(defcfun (bus-play-clocked "Bus_playClocked") :uint
  (bus bus)
  (sound-time :double)
  (sound audio-source))

(defcfun (bus-play-clocked* "Bus_playClockedEx") :uint
  (bus bus)
  (sound-time :double)
  (sound audio-source)
  (volume :float)
  (pan :float))

(defcfun (bus-play-3d "Bus_play3d") :uint
  (bus bus)
  (sound audio-source)
  (x :float)
  (y :float)
  (z :float))

(defcfun (bus-play-3d* "Bus_play3dEx") :uint
  (bus bus)
  (sound audio-source)
  (x :float)
  (y :float)
  (z :float)
  (vx :float)
  (vy :float)
  (vz :float)
  (volume :float)
  (paused :int))

(defcfun (bus-play-3d-clocked "Bus_play3dClocked") :uint
  (bus bus)
  (sound-time :double)
  (sound audio-source)
  (x :float)
  (y :float)
  (z :float))

(defcfun (bus-play-3d-clocked* "Bus_play3dClockedEx") :uint
  (bus bus)
  (sound-time :double)
  (sound audio-source)
  (x :float)
  (y :float)
  (z :float)
  (vx :float)
  (vy :float)
  (vz :float)
  (volume :float))

(defcfun (set-bus-channels "Bus_setChannels") :int
  (bus bus)
  (channels :uint))

(defcfun (set-bus-visualization "Bus_setVisualizationEnable") :void
  (bus bus)
  (enable :int))

(defcfun (bus-calc-fft "Bus_calcFFT") :pointer
  (bus bus))

(defcfun (get-bus-wave "Bus_getWave") :pointer
  (bus bus))

(defcfun (set-bus-volume "Bus_setVolume") :void
  (bus bus)
  (volume :float))

(defcfun (set-bus-looping "Bus_setLooping") :void
  (bus bus)
  (looping :int))

(defcfun (set-bus-3d-min-max-distance "Bus_set3dMinMaxDistance") :void
  (bus bus)
  (min :float)
  (max :float))

(defcfun (set-bus-3d-attenuation "Bus_set3dAttenuation") :void
  (bus bus)
  (attenuation-model attenuation-model)
  (attenuation-rolloff-factor :float))

(defcfun (set-bus-3d-doppler-factor "Bus_set3dDopplerFactor") :void
  (bus bus)
  (doppler-factor :float))

(defcfun (set-bus-3d-processing "Bus_set3dProcessing") :void
  (bus bus)
  (do-3d-processing :int))

(defcfun (set-bus-3d-listener-relative "Bus_set3dListenerRelative") :void
  (bus bus)
  (listener-relative :int))

(defcfun (set-bus-3d-distance-delay "Bus_set3dDistanceDelay") :void
  (bus bus)
  (distance-delay :int))

(defcfun (set-bus-3d-collider "Bus_set3dCollider") :void
  (bus bus)
  (audio-collider audio-collider))

(defcfun (set-bus-3d-collider* "Bus_set3dColliderEx") :void
  (bus bus)
  (audio-collider audio-collider)
  (user-data :int))

(defcfun (set-bus-3d-attenuator "Bus_set3dAttenuator") :void
  (bus bus)
  (audio-attenuator audio-attenuator))

(defcfun (set-bus-3d-inaudible-behavior "Bus_setInaudibleBehavior") :void
  (bus bus)
  (must-tick :int)
  (kill :int))

(defcfun (stop-bus "Bus_stop") :void
  (bus bus))

;;; Echo Filter
(defctype echo-filter :pointer)

(defcfun (destroy-echo-filter "EchoFilter_destroy") :void
  (echo-filter echo-filter))

(defcfun (create-echo-filter "EchoFilter_create") echo-filter)

(defcfun (set-echo-filter-params "EchoFilter_setParams") :int
  (echo-filter echo-filter)
  (delay :float))

(defcfun (set-echo-filter-params* "EchoFilter_setParamsEx") :int
  (echo-filter echo-filter)
  (delay :float)
  (decay :float)
  (filter :float))

;;; FFT Filter
(defctype fft-filter :pointer)

(defcfun (destroy-fft-filter "FFTFilter_destroy") :void
  (fft-filter fft-filter))

(defcfun (create-fft-filter "FFTFilter_create") fft-filter)

;;; Bass Boost Filter
(defctype bass-boost-filter :pointer)

(defcenum bass-boost-filter-flag
  (:wet                   0)
  (:boost                 1))

(defcfun (destroy-bass-boost-filter "BassboostFilter_destroy") :void
  (bass-boost-filter bass-boost-filter))

(defcfun (create-bass-boost-filter "BassboostFilter_create") bass-boost-filter)

(defcfun (set-bass-boost-filter-params "BassboostFilter_setParams") :int
  (bass-boost-filter bass-boost-filter)
  (boost :float))

;;; Speech
(defctype speech :pointer)

(defcfun (destroy-speech "Speech_destroy") :void
  (speech speech))

(defcfun (create-speech "Speech_create") speech)

(defcfun (set-speech-text "Speech_setText") :int
  (speech speech)
  (text :string))

(defcfun (set-speech-volume "Speech_setVolume") :void
  (speech speech)
  (volume :float))

(defcfun (set-speech-looping "Speech_setLooping") :void
  (speech speech)
  (looping :int))

(defcfun (set-speech-3d-min-max-distance "Speech_set3dMinMaxDistance") :void
  (speech speech)
  (min :float)
  (max :float))

(defcfun (set-speech-3d-attenuation "Speech_set3dAttenuation") :void
  (speech speech)
  (attenuation-model attenuation-model)
  (attenuation-rolloff-factor :float))

(defcfun (set-speech-3d-doppler-factor "Speech_set3dDopplerFactor") :void
  (speech speech)
  (doppler-factor :float))

(defcfun (set-speech-3d-processing "Speech_set3dProcessing") :void
  (speech speech)
  (do-3d-processing :int))

(defcfun (set-speech-3d-listener-relative "Speech_set3dListenerRelative") :void
  (speech speech)
  (listener-relative :int))

(defcfun (set-speech-3d-distance-delay "Speech_set3dDistanceDelay") :void
  (speech speech)
  (distance-delay :int))

(defcfun (set-speech-3d-collider "Speech_set3dCollider") :void
  (speech speech)
  (audio-collider audio-collider))

(defcfun (set-speech-3d-collider* "Speech_set3dColliderEx") :void
  (speech speech)
  (audio-collider audio-collider)
  (user-data :int))

(defcfun (set-speech-3d-attenuator "Speech_set3dAttenuator") :void
  (speech speech)
  (audio-attenuator audio-attenuator))

(defcfun (set-speech-3d-inaudible-behavior "Speech_setInaudibleBehavior") :void
  (speech speech)
  (must-tick :int)
  (kill :int))

(defcfun (set-speech-filter "Speech_setFilter") :void
  (speech speech)
  (filter-id :uint)
  (filter filter))

(defcfun (stop-speech "Speech_stop") :void
  (speech speech))

;;; Wav
(defctype wav :pointer)

(defcfun (destroy-wav "Wav_destroy") :void
  (wav wav))

(defcfun (create-wav "Wav_create") wav)

(defcfun (load-wav "Wav_load") :int
  (wav wav)
  (filename :string))

(defcfun (load-wav-mem "Wav_loadMem") :int
  (wav wav)
  (mem :pointer)
  (length :uint))

(defcfun (load-wav-mem* "Wav_loadMemEx") :int
  (wav wav)
  (mem :pointer)
  (length :uint)
  (copy :int)
  (take-ownership :int))

(defcfun (load-wav-file "Wav_loadFile") :int
  (wav wav)
  (file file))

(defcfun (get-wav-length "Wav_getLength") :double
  (wav wav))

(defcfun (set-wav-volume "Wav_setVolume") :void
  (wav wav)
  (volume :float))

(defcfun (set-wav-looping "Wav_setLooping") :void
  (wav wav)
  (looping :int))

(defcfun (set-wav-3d-min-max-distance "Wav_set3dMinMaxDistance") :void
  (wav wav)
  (min :float)
  (max :float))

(defcfun (set-wav-3d-attenuation "Wav_set3dAttenuation") :void
  (wav wav)
  (attenuation-model attenuation-model)
  (attenuation-rolloff-factor :float))

(defcfun (set-wav-3d-doppler-factor "Wav_set3dDopplerFactor") :void
  (wav wav)
  (doppler-factor :float))

(defcfun (set-wav-3d-processing "Wav_set3dProcessing") :void
  (wav wav)
  (do-3d-processing :int))

(defcfun (set-wav-3d-listener-relative "Wav_set3dListenerRelative") :void
  (wav wav)
  (listener-relative :int))

(defcfun (set-wav-3d-distance-delay "Wav_set3dDistanceDelay") :void
  (wav wav)
  (distance-delay :int))

(defcfun (set-wav-3d-collider "Wav_set3dCollider") :void
  (wav wav)
  (audio-collider audio-collider))

(defcfun (set-wav-3d-collider* "Wav_set3dColliderEx") :void
  (wav wav)
  (audio-collider audio-collider)
  (user-data :int))

(defcfun (set-wav-3d-attenuator "Wav_set3dAttenuator") :void
  (wav wav)
  (audio-attenuator audio-attenuator))

(defcfun (set-wav-3d-inaudible-behavior "Wav_setInaudibleBehavior") :void
  (wav wav)
  (must-tick :int)
  (kill :int))

(defcfun (set-wav-filter "Wav_setFilter") :void
  (wav wav)
  (filter-id :uint)
  (filter filter))

(defcfun (stop-wav "Wav_stop") :void
  (wav wav))

;;; Wav Stream
(defctype wav-stream :pointer)

(defcfun (destroy-wav-stream "WavStream_destroy") :void
  (wav-stream wav-stream))

(defcfun (create-wav-stream "WavStream_create") wav-stream)

(defcfun (load-wav-stream "WavStream_load") :int
  (wav-stream wav-stream)
  (filename :string))

(defcfun (load-wav-stream-mem "WavStream_loadMem") :int
  (wav-stream wav-stream)
  (mem :pointer)
  (length :uint))

(defcfun (load-wav-stream-mem* "WavStream_loadMemEx") :int
  (wav-stream wav-stream)
  (mem :pointer)
  (length :uint)
  (copy :int)
  (take-ownership :int))

(defcfun (load-wav-stream-to-mem "WavStream_loadToMem") :int
  (wav-stream wav-stream)
  (filename :string))

(defcfun (load-wav-stream-file "WavStream_loadFile") :int
  (wav-stream wav-stream)
  (file file))

(defcfun (load-wav-stream-file-to-mem "WavStream_loadFileToMem") :int
  (wav-stream wav-stream)
  (file file))

(defcfun (get-wav-stream-length "WavStream_getLength") :double
  (wav wav))

(defcfun (set-wav-stream-volume "WavStream_setVolume") :void
  (wav-stream wav-stream)
  (volume :float))

(defcfun (set-wav-stream-looping "WavStream_setLooping") :void
  (wav-stream wav-stream)
  (looping :int))

(defcfun (set-wav-stream-3d-min-max-distance "WavStream_set3dMinMaxDistance") :void
  (wav-stream wav-stream)
  (min :float)
  (max :float))

(defcfun (set-wav-stream-3d-attenuation "WavStream_set3dAttenuation") :void
  (wav-stream wav-stream)
  (attenuation-model attenuation-model)
  (attenuation-rolloff-factor :float))

(defcfun (set-wav-stream-3d-doppler-factor "WavStream_set3dDopplerFactor") :void
  (wav-stream wav-stream)
  (doppler-factor :float))

(defcfun (set-wav-stream-3d-processing "WavStream_set3dProcessing") :void
  (wav-stream wav-stream)
  (do-3d-processing :int))

(defcfun (set-wav-stream-3d-listener-relative "WavStream_set3dListenerRelative") :void
  (wav-stream wav-stream)
  (listener-relative :int))

(defcfun (set-wav-stream-3d-distance-delay "WavStream_set3dDistanceDelay") :void
  (wav-stream wav-stream)
  (distance-delay :int))

(defcfun (set-wav-stream-3d-collider "WavStream_set3dCollider") :void
  (wav-stream wav-stream)
  (audio-collider audio-collider))

(defcfun (set-wav-stream-3d-collider* "WavStream_set3dColliderEx") :void
  (wav-stream wav-stream)
  (audio-collider audio-collider)
  (user-data :int))

(defcfun (set-wav-stream-3d-attenuator "WavStream_set3dAttenuator") :void
  (wav-stream wav-stream)
  (audio-attenuator audio-attenuator))

(defcfun (set-wav-stream-3d-inaudible-behavior "WavStream_setInaudibleBehavior") :void
  (wav-stream wav-stream)
  (must-tick :int)
  (kill :int))

(defcfun (set-wav-stream-filter "WavStream_setFilter") :void
  (wav-stream wav-stream)
  (filter-id :uint)
  (filter filter))

(defcfun (stop-wav-stream "WavStream_stop") :void
  (wav-stream wav-stream))

;;; Prg
(defctype prg :pointer)

(defcfun (destroy-prg "Prg_destroy") :void
  (prg prg))

(defcfun (create-prg "Prg_create") prg)

(defcfun (prg-random "Prg_rand") :uint
  (prg prg))

(defcfun (prg-srandom "Prg_srand") :void
  (prg prg)
  (seed :int))

;;; Sfxr
(defctype sfxr :pointer)

(defcenum sfxr-preset
  (:coin                  0)
  (:laser                 1)
  (:explosion             2)
  (:powerup               3)
  (:hurt                  4)
  (:jump                  5)
  (:blip                  6))

(defcfun (destroy-sfxr "Sfxr_destroy") :void
  (sfxr sfxr))

(defcfun (create-sfxr "Sfxr_create") sfxr)

(defcfun (load-sfxr-params "Sfxr_loadParams") :int
  (sfxr sfxr)
  (filename :string))

(defcfun (load-sfxr-params-mem "Sfxr_loadParamsMem") :int
  (sfxr sfxr)
  (mem :pointer)
  (length :uint))

(defcfun (load-sfxr-params-mem* "Sfxr_loadParamsMemEx") :int
  (sfxr sfxr)
  (mem :pointer)
  (length :uint)
  (copy :int)
  (take-ownership :int))

(defcfun (load-sfxr-params-file "Sfxr_loadParamsFile") :int
  (sfxr sfxr)
  (file file))

(defcfun (load-sfxr-preset "Sfxr_loadPreset") :int
  (sfxr sfxr)
  (preset-no sfxr-preset)
  (seed :float))

(defcfun (set-sfxr-volume "Sfxr_setVolume") :void
  (sfxr sfxr)
  (volume :float))

(defcfun (set-sfxr-looping "Sfxr_setLooping") :void
  (sfxr sfxr)
  (looping :int))

(defcfun (set-sfxr-3d-min-max-distance "Sfxr_set3dMinMaxDistance") :void
  (sfxr sfxr)
  (min :float)
  (max :float))

(defcfun (set-sfxr-3d-attenuation "Sfxr_set3dAttenuation") :void
  (sfxr sfxr)
  (attenuation-model attenuation-model)
  (attenuation-rolloff-factor :float))

(defcfun (set-sfxr-3d-doppler-factor "Sfxr_set3dDopplerFactor") :void
  (sfxr sfxr)
  (doppler-factor :float))

(defcfun (set-sfxr-3d-processing "Sfxr_set3dProcessing") :void
  (sfxr sfxr)
  (do-3d-processing :int))

(defcfun (set-sfxr-3d-listener-relative "Sfxr_set3dListenerRelative") :void
  (sfxr sfxr)
  (listener-relative :int))

(defcfun (set-sfxr-3d-distance-delay "Sfxr_set3dDistanceDelay") :void
  (sfxr sfxr)
  (distance-delay :int))

(defcfun (set-sfxr-3d-collider "Sfxr_set3dCollider") :void
  (sfxr sfxr)
  (audio-collider audio-collider))

(defcfun (set-sfxr-3d-collider* "Sfxr_set3dColliderEx") :void
  (sfxr sfxr)
  (audio-collider audio-collider)
  (user-data :int))

(defcfun (set-sfxr-3d-attenuator "Sfxr_set3dAttenuator") :void
  (sfxr sfxr)
  (audio-attenuator audio-attenuator))

(defcfun (set-sfxr-3d-inaudible-behavior "Sfxr_setInaudibleBehavior") :void
  (sfxr sfxr)
  (must-tick :int)
  (kill :int))

(defcfun (set-sfxr-filter "Sfxr_setFilter") :void
  (sfxr sfxr)
  (filter-id :uint)
  (filter filter))

(defcfun (stop-sfxr "Sfxr_stop") :void
  (sfxr sfxr))

;;; Flanger Filter
(defctype flanger-filter :pointer)

(defcenum flanger-filter-flag
  (:wet                   0)
  (:delay                 1)
  (:freq                  2))

(defcfun (destroy-flanger-filter "FlangerFilter_destroy") :void
  (flanger-filter flanger-filter))

(defcfun (create-flanger-filter "FlangerFilter_create") flanger-filter)

(defcfun (set-flanger-filter-params "FlangerFilter_setParams") :int
  (flanger-filter flanger-filter)
  (delay :float)
  (freq :float))

;;; DC Removal Filter
(defctype dc-removal-filter :pointer)

(defcfun (destroy-dc-removal-filter "DCRemovalFilter_destroy") :void
  (dc-removal-filter dc-removal-filter))

(defcfun (create-dc-removal-filter "DCRemovalFilter_create") dc-removal-filter)

(defcfun (set-dc-removal-filter-params "DCRemovalFilter_setParams") :int
  (dc-removal-filter dc-removal-filter))

(defcfun (set-dc-removal-filter-params* "DCRemovalFilter_setParamsEx") :int
  (dc-removal-filter dc-removal-filter)
  (length :float))

;;; OpenMPT
(defctype openmpt :pointer)

(defcfun (destroy-openmpt "Openmpt_destroy") :void
  (openmpt openmpt))

(defcfun (create-openmpt "Openmpt_create") openmpt)

(defcfun (load-openmpt "Openmpt_load") :int
  (openmpt openmpt)
  (filename :string))

(defcfun (load-openmpt-mem "Openmpt_loadMem") :int
  (openmpt openmpt)
  (mem :pointer)
  (length :uint))

(defcfun (load-openmpt-mem* "Openmpt_loadMemEx") :int
  (openmpt openmpt)
  (mem :pointer)
  (length :uint)
  (copy :int)
  (take-ownership :int))

(defcfun (load-openmpt-file "Openmpt_loadFile") :int
  (openmpt openmpt)
  (file file))

(defcfun (set-openmpt-volume "Openmpt_setVolume") :void
  (openmpt openmpt)
  (volume :float))

(defcfun (set-openmpt-looping "Openmpt_setLooping") :void
  (openmpt openmpt)
  (looping :int))

(defcfun (set-openmpt-3d-min-max-distance "Openmpt_set3dMinMaxDistance") :void
  (openmpt openmpt)
  (min :float)
  (max :float))

(defcfun (set-openmpt-3d-attenuation "Openmpt_set3dAttenuation") :void
  (openmpt openmpt)
  (attenuation-model attenuation-model)
  (attenuation-rolloff-factor :float))

(defcfun (set-openmpt-3d-doppler-factor "Openmpt_set3dDopplerFactor") :void
  (openmpt openmpt)
  (doppler-factor :float))

(defcfun (set-openmpt-3d-processing "Openmpt_set3dProcessing") :void
  (openmpt openmpt)
  (do-3d-processing :int))

(defcfun (set-openmpt-3d-listener-relative "Openmpt_set3dListenerRelative") :void
  (openmpt openmpt)
  (listener-relative :int))

(defcfun (set-openmpt-3d-distance-delay "Openmpt_set3dDistanceDelay") :void
  (openmpt openmpt)
  (distance-delay :int))

(defcfun (set-openmpt-3d-collider "Openmpt_set3dCollider") :void
  (openmpt openmpt)
  (audio-collider audio-collider))

(defcfun (set-openmpt-3d-collider* "Openmpt_set3dColliderEx") :void
  (openmpt openmpt)
  (audio-collider audio-collider)
  (user-data :int))

(defcfun (set-openmpt-3d-attenuator "Openmpt_set3dAttenuator") :void
  (openmpt openmpt)
  (audio-attenuator audio-attenuator))

(defcfun (set-openmpt-3d-inaudible-behavior "Openmpt_setInaudibleBehavior") :void
  (openmpt openmpt)
  (must-tick :int)
  (kill :int))

(defcfun (set-openmpt-filter "Openmpt_setFilter") :void
  (openmpt openmpt)
  (filter-id :uint)
  (filter filter))

(defcfun (stop-openmpt "Openmpt_stop") :void
  (openmpt openmpt))

;;; Monotone
(defctype monotone :pointer)

(defcenum monotone-waveform
  (:square                0)
  (:saw                   1)
  (:sin                   2)
  (:sawsin                3))

(defcfun (destroy-monotone "Monotone_destroy") :void
  (monotone monotone))

(defcfun (create-monotone "Monotone_create") monotone)

(defcfun (set-monotone-params "Monotone_setParams") :int
  (monotone monotone)
  (hardware-channels :int))

(defcfun (set-monotone-params* "Monotone_setParamsEx") :int
  (monotone monotone)
  (hardware-channels :int)
  (wave-form monotone-waveform))

(defcfun (load-monotone "Monotone_load") :int
  (monotone monotone)
  (filename :string))

(defcfun (load-monotone-mem "Monotone_loadMem") :int
  (monotone monotone)
  (mem :pointer)
  (length :uint))

(defcfun (load-monotone-mem* "Monotone_loadMemEx") :int
  (monotone monotone)
  (mem :pointer)
  (length :uint)
  (copy :int)
  (take-ownership :int))

(defcfun (load-monotone-file "Monotone_loadFile") :int
  (monotone monotone)
  (file file))

(defcfun (set-monotone-volume "Monotone_setVolume") :void
  (monotone monotone)
  (volume :float))

(defcfun (set-monotone-looping "Monotone_setLooping") :void
  (monotone monotone)
  (looping :int))

(defcfun (set-monotone-3d-min-max-distance "Monotone_set3dMinMaxDistance") :void
  (monotone monotone)
  (min :float)
  (max :float))

(defcfun (set-monotone-3d-attenuation "Monotone_set3dAttenuation") :void
  (monotone monotone)
  (attenuation-model attenuation-model)
  (attenuation-rolloff-factor :float))

(defcfun (set-monotone-3d-doppler-factor "Monotone_set3dDopplerFactor") :void
  (monotone monotone)
  (doppler-factor :float))

(defcfun (set-monotone-3d-processing "Monotone_set3dProcessing") :void
  (monotone monotone)
  (do-3d-processing :int))

(defcfun (set-monotone-3d-listener-relative "Monotone_set3dListenerRelative") :void
  (monotone monotone)
  (listener-relative :int))

(defcfun (set-monotone-3d-distance-delay "Monotone_set3dDistanceDelay") :void
  (monotone monotone)
  (distance-delay :int))

(defcfun (set-monotone-3d-collider "Monotone_set3dCollider") :void
  (monotone monotone)
  (audio-collider audio-collider))

(defcfun (set-monotone-3d-collider* "Monotone_set3dColliderEx") :void
  (monotone monotone)
  (audio-collider audio-collider)
  (user-data :int))

(defcfun (set-monotone-3d-attenuator "Monotone_set3dAttenuator") :void
  (monotone monotone)
  (audio-attenuator audio-attenuator))

(defcfun (set-monotone-3d-inaudible-behavior "Monotone_setInaudibleBehavior") :void
  (monotone monotone)
  (must-tick :int)
  (kill :int))

(defcfun (set-monotone-filter "Monotone_setFilter") :void
  (monotone monotone)
  (filter-id :uint)
  (filter filter))

(defcfun (stop-monotone "Monotone_stop") :void
  (monotone monotone))

;;; TedSid
(defctype ted-sid :pointer)

(defcfun (destroy-ted-sid "TedSid_destroy") :void
  (ted-sid ted-sid))

(defcfun (create-ted-sid "TedSid_create") ted-sid)

(defcfun (load-ted-sid "TedSid_load") :int
  (ted-sid ted-sid)
  (filename :string))

(defcfun (load-ted-sid-to-mem "TedSid_loadToMem") :int
  (ted-sid ted-sid)
  (filename :string))

(defcfun (load-ted-sid-mem "TedSid_loadMem") :int
  (ted-sid ted-sid)
  (mem :pointer)
  (length :uint))

(defcfun (load-ted-sid-mem* "TedSid_loadMemEx") :int
  (ted-sid ted-sid)
  (mem :pointer)
  (length :uint)
  (copy :int)
  (take-ownership :int))

(defcfun (load-ted-sid-file "TedSid_loadFile") :int
  (ted-sid ted-sid)
  (file file))

(defcfun (load-ted-sid-file-to-mem "TedSid_loadFileToMem") :int
  (ted-sid ted-sid)
  (file file))

(defcfun (set-ted-sid-volume "TedSid_setVolume") :void
  (ted-sid ted-sid)
  (volume :float))

(defcfun (set-ted-sid-looping "TedSid_setLooping") :void
  (ted-sid ted-sid)
  (looping :int))

(defcfun (set-ted-sid-3d-min-max-distance "TedSid_set3dMinMaxDistance") :void
  (ted-sid ted-sid)
  (min :float)
  (max :float))

(defcfun (set-ted-sid-3d-attenuation "TedSid_set3dAttenuation") :void
  (ted-sid ted-sid)
  (attenuation-model attenuation-model)
  (attenuation-rolloff-factor :float))

(defcfun (set-ted-sid-3d-doppler-factor "TedSid_set3dDopplerFactor") :void
  (ted-sid ted-sid)
  (doppler-factor :float))

(defcfun (set-ted-sid-3d-processing "TedSid_set3dProcessing") :void
  (ted-sid ted-sid)
  (do-3d-processing :int))

(defcfun (set-ted-sid-3d-listener-relative "TedSid_set3dListenerRelative") :void
  (ted-sid ted-sid)
  (listener-relative :int))

(defcfun (set-ted-sid-3d-distance-delay "TedSid_set3dDistanceDelay") :void
  (ted-sid ted-sid)
  (distance-delay :int))

(defcfun (set-ted-sid-3d-collider "TedSid_set3dCollider") :void
  (ted-sid ted-sid)
  (audio-collider audio-collider))

(defcfun (set-ted-sid-3d-collider* "TedSid_set3dColliderEx") :void
  (ted-sid ted-sid)
  (audio-collider audio-collider)
  (user-data :int))

(defcfun (set-ted-sid-3d-attenuator "TedSid_set3dAttenuator") :void
  (ted-sid ted-sid)
  (audio-attenuator audio-attenuator))

(defcfun (set-ted-sid-3d-inaudible-behavior "TedSid_setInaudibleBehavior") :void
  (ted-sid ted-sid)
  (must-tick :int)
  (kill :int))

(defcfun (set-ted-sid-filter "TedSid_setFilter") :void
  (ted-sid ted-sid)
  (filter-id :uint)
  (filter filter))

(defcfun (stop-ted-sid "TedSid_stop") :void
  (ted-sid ted-sid))

;;; VirtualFilter
(defctype virtual-filter :pointer)

(defcfun (destroy-virtual-filter "VirtualFilter_destroy") :void
  (virtual-filter virtual-filter))

(defcfun (create-virtual-filter "VirtualFilter_create") virtual-filter)

(defcfun (set-virtual-filter-filter "VirtualFilter_setFilterCallback") :void
  (function :pointer))

(defcfun (set-virtual-filter-filter-channel "VirtualFilter_setFilterChannelCallback") :void
  (function :pointer))

;;; VirtualAudioSource
(defctype virtual-audio-source :pointer)

(defcfun (destroy-virtual-audio-source "VirtualAudioSource_destroy") :void
  (virtual-audio-source virtual-audio-source))

(defcfun (create-virtual-audio-source "VirtualAudioSource_create") virtual-audio-source)

(defcfun (set-virtual-audio-source-get-audio "VirtualAudioSource_setGetAudioCallback") :void
  (function :pointer))

(defcfun (set-virtual-audio-source-has-ended "VirtualAudioSource_setHasEndedCallback") :void
  (function :pointer))

(defcfun (set-virtual-audio-source-seek "VirtualAudioSource_setSeekCallback") :void
  (function :pointer))

(defcfun (set-virtual-audio-source-rewind "VirtualAudioSource_setRewindCallback") :void
  (function :pointer))

(defcfun (set-virtual-audio-source-get-info "VirtualAudioSource_setGetInfoCallback") :void
  (function :pointer))

(defcfun (set-virtual-audio-source-volume "VirtualAudioSource_setVolume") :void
  (virtual-audio-source virtual-audio-source)
  (volume :float))

(defcfun (set-virtual-audio-source-looping "VirtualAudioSource_setLooping") :void
  (virtual-audio-source virtual-audio-source)
  (looping :int))

(defcfun (set-virtual-audio-source-3d-min-max-distance "VirtualAudioSource_set3dMinMaxDistance") :void
  (virtual-audio-source virtual-audio-source)
  (min :float)
  (max :float))

(defcfun (set-virtual-audio-source-3d-attenuation "VirtualAudioSource_set3dAttenuation") :void
  (virtual-audio-source virtual-audio-source)
  (attenuation-model attenuation-model)
  (attenuation-rolloff-factor :float))

(defcfun (set-virtual-audio-source-3d-doppler-factor "VirtualAudioSource_set3dDopplerFactor") :void
  (virtual-audio-source virtual-audio-source)
  (doppler-factor :float))

(defcfun (set-virtual-audio-source-3d-processing "VirtualAudioSource_set3dProcessing") :void
  (virtual-audio-source virtual-audio-source)
  (do-3d-processing :int))

(defcfun (set-virtual-audio-source-3d-listener-relative "VirtualAudioSource_set3dListenerRelative") :void
  (virtual-audio-source virtual-audio-source)
  (listener-relative :int))

(defcfun (set-virtual-audio-source-3d-distance-delay "VirtualAudioSource_set3dDistanceDelay") :void
  (virtual-audio-source virtual-audio-source)
  (distance-delay :int))

(defcfun (set-virtual-audio-source-3d-collider "VirtualAudioSource_set3dCollider") :void
  (virtual-audio-source virtual-audio-source)
  (audio-collider audio-collider))

(defcfun (set-virtual-audio-source-3d-collider* "VirtualAudioSource_set3dColliderEx") :void
  (virtual-audio-source virtual-audio-source)
  (audio-collider audio-collider)
  (user-data :int))

(defcfun (set-virtual-audio-source-3d-attenuator "VirtualAudioSource_set3dAttenuator") :void
  (virtual-audio-source virtual-audio-source)
  (audio-attenuator audio-attenuator))

(defcfun (set-virtual-audio-source-3d-inaudible-behavior "VirtualAudioSource_setInaudibleBehavior") :void
  (virtual-audio-source virtual-audio-source)
  (must-tick :int)
  (kill :int))

(defcfun (set-virtual-audio-source-filter "VirtualAudioSource_setFilter") :void
  (virtual-audio-source virtual-audio-source)
  (filter-id :uint)
  (filter filter))

(defcfun (stop-virtual-audio-source "VirtualAudioSource_stop") :void
  (virtual-audio-source virtual-audio-source))

(defcfun (get-virtual-audio-source-base-samplerate "VirtualAudioSource_getBaseSamplerate") :float
  (virtual-audio-source virtual-audio-source))

(defcfun (set-virtual-audio-source-base-samplerate "VirtualAudioSource_setBaseSamplerate") :void
  (virtual-audio-source virtual-audio-source)
  (samplerate :float))

;;; VirtualAudioCollider
(defctype virtual-audio-collider :pointer)

(defcfun (destroy-virtual-audio-collider "VirtualAudioCollider_destroy") :void
  (virtual-audio-collider virtual-audio-collider))

(defcfun (create-virtual-audio-collider "VirtualAudioCollider_create") virtual-audio-collider)

(defcfun (set-virtual-audio-collider-collide "VirtualAudioCollider_setCollideCallback") :void
  (function :pointer))

;;; VirtualAudioAttenuator
(defctype virtual-audio-attenuator :pointer)

(defcfun (destroy-virtual-audio-attenuator "VirtualAudioAttenuator_destroy") :void
  (virtual-audio-attenuator virtual-audio-attenuator))

(defcfun (create-virtual-audio-attenuator "VirtualAudioAttenuator_create") virtual-audio-attenuator)

(defcfun (set-virtual-audio-attenuator-attenuate "VirtualAudioAttenuator_setAttenuateCallback") :void
  (function :pointer))

;;; AudioSourceInstance3dData
(defctype audio-source-instance-3d-data :pointer)

(defcfun (get-audio-source-instance-3d-data-position "AudioSourceInstance3dData_get3dPosition") :void
  (instance audio-source-instance-3d-data)
  (x :pointer)
  (y :pointer)
  (z :pointer))

(defcfun (get-audio-source-instance-3d-data-velocity "AudioSourceInstance3dData_get3dVelocity") :void
  (instance audio-source-instance-3d-data)
  (x :pointer)
  (y :pointer)
  (z :pointer))

(defcfun (get-audio-source-instance-3d-data-min-distance "AudioSourceInstance3dData_get3dMinDistance") :float
  (instance audio-source-instance-3d-data))

(defcfun (get-audio-source-instance-3d-data-max-distance "AudioSourceInstance3dData_get3dMaxDistance") :float
  (instance audio-source-instance-3d-data))

(defcfun (get-audio-source-instance-3d-data-attenuation-rolloff "AudioSourceInstance3dData_get3dAttenuationRolloff") :float
  (instance audio-source-instance-3d-data))

(defcfun (get-audio-source-instance-3d-data-attenuation-model "AudioSourceInstance3dData_get3dAttenuationModel") attenuation-model
  (instance audio-source-instance-3d-data))

(defcfun (get-audio-source-instance-3d-data-doppler-factor "AudioSourceInstance3dData_get3dDopplerFactor") :float
  (instance audio-source-instance-3d-data))

(defcfun (get-audio-source-instance-3d-data-audio-collider "AudioSourceInstance3dData_getAudioCollider") audio-collider
  (instance audio-source-instance-3d-data))

(defcfun (get-audio-source-instance-3d-data-audio-attenuator "AudioSourceInstance3dData_getAudioAttenuator") audio-attenuator
  (instance audio-source-instance-3d-data))

(defcfun (get-audio-source-instance-3d-data-collider-data "AudioSourceInstance3dData_getColliderData") :int
  (instance audio-source-instance-3d-data))

(defcfun (get-audio-source-instance-3d-data-doppler-value "AudioSourceInstance3dData_getDopplerValue") :float
  (instance audio-source-instance-3d-data))

(defcfun (get-audio-source-instance-3d-data-volume "AudioSourceInstance3dData_get3dVolume") :float
  (instance audio-source-instance-3d-data))

(defcfun (get-audio-source-instance-3d-data-channel-volume "AudioSourceInstance3dData_getChannelVolume") :float
  (instance audio-source-instance-3d-data)
  (channel :uint))

(defcfun (get-audio-source-instance-3d-data-flags "AudioSourceInstance3dData_getFlags") :uint
  (instance audio-source-instance-3d-data))

(defcfun (get-audio-source-instance-3d-data-handle "AudioSourceInstance3dData_getHandle") :uint
  (instance audio-source-instance-3d-data))

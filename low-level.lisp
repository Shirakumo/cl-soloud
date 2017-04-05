#|
 This file is a part of cl-soloud
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.soloud.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

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

(defcenum soloud-backend
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

(defcenum biquad-resonant-filter-pass
  (:none                  0)
  (:lowpass               1)
  (:highpass              2)
  (:bandpass              3))

(defcenum biquad-resonant-filter-flag
  (:wet                   0)
  (:samplerate            1)
  (:frequency             2)
  (:resonance             3))

(defcenum lofi-filter-flag
  (:wet                   0)
  (:samplerate            1)
  (:bitdepth              2))

(defcenum bass-boost-filter-flag
  (:wet                   0)
  (:boost                 1))

(defcenum sfxr-effect
  (:coin                  0)
  (:laser                 1)
  (:explosion             2)
  (:powerup               3)
  (:hurt                  4)
  (:jump                  5)
  (:blip                  6))

(defcenum flanger-filter-flag
  (:wet                   0)
  (:delay                 1)
  (:freq                  2))

(defcenum monotone-wave
  (:square                0)
  (:saw                   1)
  (:sin                   2)
  (:sawsin                3))

(defctype aligned-float-buffer :pointer)
(defctype soloud :pointer)
(defctype audio-collider :pointer)
(defctype audio-attenuator :pointer)
(defctype audio-source :pointer)
(defctype biquad-resonant-filter :pointer)
(defctype lofi-filter :pointer)
(defctype bus :pointer)
(defctype echo-filter :pointer)
(defctype fader :pointer)
(defctype fft-filter :pointer)
(defctype bass-boost-filter :pointer)
(defctype filter :pointer)
(defctype speech :pointer)
(defctype wav :pointer)
(defctype wav-stream :pointer)
(defctype prg :pointer)
(defctype sfxr :pointer)
(defctype flanger-filter :pointer)
(defctype dc-removal-filter :pointer)
(defctype open-mpt :pointer)
(defctype monotone :pointer)
(defctype ted-sid :pointer)
(defctype file :pointer)

(defcfun (create "Soloud_create") soloud
  )

(defcfun (destroy "Soloud_destroy") :void
  (soloud soloud))

(defcfun (init "Soloud_init") :void
  (soloud soloud))

(defcfun (init-ex "Soloud_initEx") :void
  (soloud soloud)
  (flags soloud-flag)
  (backend soloud-backend)
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

(defcfun (get-backend-id "Soloud_getBackendId") soloud-backend
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

(defcfun (get-filter-parameter "Soloud_getFilterParameter") :gloat
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

(defcfun (get-active-voice-count "Soloud_getActiveVoiceCount") :uint
  (soloud soloud)
  (voice-handle :uint))

(defcfun (is-valid-voice-handle "Soloud_isValidVoiceHandle") :int
  (soloud soloud)
  (voice-handle :uint))

(defcfun (get-relative-play-speed "Soloud_getRelativePlayPseed") :float
  (soloud soloud)
  (voice-handle :uint))

(defcfun (get-post-clip-scaler "Soloud_getPostClipScaler") :float
  (soloud soloud))

(defcfun (get-global-volume "Soloud_getGlobalVolume") :float
  (soloud soloud))

(defcfun (get-max-active-voice-count "Soloud_getMaxActiveVoiceCount") :uint
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
  (int :protect))

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
  (float :volume))

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
  (voice-handle :uint)
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
  (to :fload)
  (time :double))

(defcfun (oscillate-pan "Soloud_oscillatePan") :void
  (soloud soloud)
  (voice-handle :uint)
  (from :float)
  (to :fload)
  (time :double))

(defcfun (oscillate-relative-play-speed "Soloud_oscillateRelativePlaySpeed") :void
  (soloud soloud)
  (voice-handle :uint)
  (from :float)
  (to :fload)
  (time :double))

(defcfun (oscillate-global-volume "Soloud_oscillateGlobalVolume") :void
  (soloud soloud)
  (voice-handle :uint)
  (from :float)
  (to :fload)
  (time :double))

(defcfun (set-global-filter "Soloud_setGlobalFilter") :void
  (soloud soloud)
  (filter-id :uint)
  (filter filter))

(defcfun (init "Soloud_setVisualizationEnable") :void
  (soloud soloud)
  (enable :int))

(defcfun (init "Soloud_calcFFT") :pointer
  (soloud soloud))

(defcfun (init "Soloud_getWave") :pointer
  (soloud soloud))

(defcfun (init "Soloud_getLoopCount") :uint
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
  (attenuation-model :uint)
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

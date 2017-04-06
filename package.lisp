#|
 This file is a part of cl-soloud
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:cl-soloud-cffi
  (:nicknames #:org.shirakumo.fraf.soloud.cffi)
  (:use #:cl #:cffi)
  ;; low-level.lisp
  (:shadow :open :close :continue)
  (:export
   #:*static*
   #:*max-filters*
   #:libsoloud
   ;; SoLoud
   #:soloud
   #:audio-collider
   #:audio-attenuator
   #:audio-source
   #:file
   #:filter
   #:soloud-backend
   #:soloud-flag
   #:soloud-attenuation
   #:create
   #:destroy
   #:init
   #:init*
   #:deinit
   #:get-version
   #:get-error-string
   #:get-backend-id
   #:get-backend-string
   #:get-backend-channels
   #:get-backend-sample-rate
   #:get-backend-buffer-size
   #:set-speaker-position
   #:play
   #:play*
   #:play-clocked
   #:play-clocked*
   #:play-3d
   #:play-3d*
   #:play-3d-clocked
   #:play-3d-clocked*
   #:seek
   #:stop
   #:stop-all
   #:stop-audio-source
   #:set-filter-parameter
   #:get-filter-parameter
   #:fade-filter-parameter
   #:oscillate-filter-parameter
   #:get-stream-time
   #:get-pause
   #:get-volume
   #:get-overall-volume
   #:get-pan
   #:get-sample-rate
   #:get-protect-voice
   #:get-active-voice-count
   #:is-valid-voice-handle
   #:get-relative-play-speed
   #:get-post-clip-scaler
   #:get-global-volume
   #:get-max-active-voice-count
   #:get-looping
   #:set-looping
   #:set-max-active-voice-count
   #:set-inaudible-behavior
   #:set-global-volume
   #:set-post-clip-scaler
   #:set-pause
   #:set-pause-all
   #:set-relative-play-speed
   #:set-protect-voice
   #:set-sample-rate
   #:set-pan
   #:set-pan-absolute
   #:set-pan-absolute*
   #:set-volume
   #:set-delay-samples
   #:fade-volume
   #:fade-pan
   #:fade-relative-play-speed
   #:fade-global-volume
   #:schedule-pause
   #:schedule-stop
   #:oscillate-volume
   #:oscillate-pan
   #:oscillate-relative-play-speed
   #:oscillate-global-volume
   #:set-global-filter
   #:set-visualization
   #:calc-fft
   #:get-wave
   #:get-loop-count
   #:get-info
   #:create-voice-group
   #:destroy-voice-group
   #:add-voice-to-group
   #:is-voice-group
   #:is-voice-group-empty
   #:update-3d-audio
   #:set-3d-sound-speed
   #:get-3d-sound-speed
   #:set-3d-listener-parameters
   #:set-3d-listener-parameters*
   #:set-3d-listener-position
   #:set-3d-listener-at
   #:set-3d-listener-up
   #:set-3d-listener-velocity
   #:set-3d-source-parameters
   #:set-3d-source-parameters*
   #:set-3d-source-position
   #:set-3d-source-velocity
   #:set-3d-source-min-max-distance
   #:set-3d-source-attenuation
   #:set-3d-source-doppler-factor
   #:mix
   #:mix-signed-16
   ;; Audio Attenuator
   #:destroy-audio-attenuator
   #:attenuate-audio-attenuator
   ;; Biquad Resonant Filter
   #:biquad-resonant-filter-type
   #:biquad-resonant-filter-flag
   #:biquad-resonant-filter
   #:destroy-biquad-resonant-filter
   #:create-biquad-resonant-filter
   #:set-biquad-resonant-filter-params
   ;; Lofi Filter
   #:lofi-filter-flag
   #:lofi-filter
   #:destroy-lofi-filter
   #:create-lofi-filter
   #:set-lofi-filter-params
   ;; Bus
   #:bus
   #:destroy-bus
   #:create-bus
   #:set-bus-filter
   #:bus-play
   #:bus-play*
   #:bus-play-clocked
   #:bus-play-clocked*
   #:bus-play-3d
   #:bus-play-3d*
   #:bus-play-3d-clocked
   #:bus-play-3d-clocked*
   #:set-bus-channels
   #:set-bus-visualization
   #:bus-calc-fft
   #:get-bus-wave
   #:set-bus-volume
   #:set-bus-looping
   #:set-bus-3d-min-max-distance
   #:set-bus-3d-attenuation
   #:set-bus-3d-doppler-factor
   #:set-bus-3d-processing
   #:set-bus-3d-listener-relative
   #:set-bus-3d-distance-delay
   #:set-bus-3d-collider
   #:set-bus-3d-collider*
   #:set-bus-3d-attenuator
   #:set-bus-3d-inaudible-behavior
   #:stop-bus
   ;; Echo Filter
   #:echo-filter
   #:destroy-echo-filter
   #:create-echo-filter
   #:set-echo-filter-params
   #:set-echo-filter-params*
   ;; FFT Filter
   #:fft-filter
   #:destroy-fft-filter
   #:create-fft-filter
   ;; Bass Boost Filter
   #:bass-boost-filter-flag
   #:bass-boost-filter
   #:destroy-bass-boost-filter
   #:create-bass-boost-filter
   #:set-bass-boost-filter-params
   ;; Speech
   #:speech
   #:destroy-speech
   #:create-speech
   #:set-speech-text
   #:set-speech-volume
   #:set-speech-looping
   #:set-speech-3d-min-max-distance
   #:set-speech-3d-attenuation
   #:set-speech-3d-doppler-factor
   #:set-speech-3d-processing
   #:set-speech-3d-listener-relative
   #:set-speech-3d-distance-delay
   #:set-speech-3d-collider
   #:set-speech-3d-collider*
   #:set-speech-3d-attenuator
   #:set-speech-3d-inaudible-behavior
   #:set-speech-filter
   #:stop-speech
   ;; Wav
   #:wav
   #:destroy-wav
   #:create-wav
   #:load-wav
   #:load-wav-mem
   #:load-wav-mem*
   #:load-wav-file
   #:get-wav-length
   #:set-wav-volume
   #:set-wav-looping
   #:set-wav-3d-min-max-distance
   #:set-wav-3d-attenuation
   #:set-wav-3d-doppler-factor
   #:set-wav-3d-processing
   #:set-wav-3d-listener-relative
   #:set-wav-3d-distance-delay
   #:set-wav-3d-collider
   #:set-wav-3d-collider*
   #:set-wav-3d-attenuator
   #:set-wav-3d-inaudible-behavior
   #:set-wav-filter
   #:stop-wav
   ;; Wav Stream
   #:wav-stream
   #:destroy-wav-stream
   #:create-wav-stream
   #:load-wav-stream
   #:load-wav-stream-mem
   #:load-wav-stream-mem*
   #:load-wav-stream-to-mem
   #:load-wav-stream-file
   #:wav-stream-file-load-to-mem
   #:get-wav-stream-length
   #:set-wav-stream-volume
   #:set-wav-stream-looping
   #:set-wav-stream-3d-min-max-distance
   #:set-wav-stream-3d-attenuation
   #:set-wav-stream-3d-doppler-factor
   #:set-wav-stream-3d-processing
   #:set-wav-stream-3d-listener-relative
   #:set-wav-stream-3d-distance-delay
   #:set-wav-stream-3d-collider
   #:set-wav-stream-3d-collider*
   #:set-wav-stream-3d-attenuator
   #:set-wav-stream-3d-inaudible-behavior
   #:set-wav-stream-filter
   #:stop-wav-stream
   ;; Prg
   #:org
   #:destroy-prg
   #:create-prg
   #:prg-random
   #:prg-srandom
   ;; Sfxr
   #:sfxr
   #:sfxr-preset
   #:destroy-sfxr
   #:create-sfxr
   #:load-sfxr-params
   #:load-sfxr-params-mem
   #:load-sfxr-params-mem*
   #:load-sfxr-params-file
   #:load-sfxr-preset
   #:set-sfxr-volume
   #:set-sfxr-looping
   #:set-sfxr-3d-min-max-distance
   #:set-sfxr-3d-attenuation
   #:set-sfxr-3d-doppler-factor
   #:set-sfxr-3d-processing
   #:set-sfxr-3d-listener-relative
   #:set-sfxr-3d-distance-delay
   #:set-sfxr-3d-collider
   #:set-sfxr-3d-collider*
   #:set-sfxr-3d-attenuator
   #:set-sfxr-3d-inaudible-behavior
   #:set-sfxr-filter
   #:stop-sfxr
   ;; Flanger Filter
   #:flanger-filter
   #:flanger-filter-flag
   #:destroy-flanger-filter
   #:create-flanger-filter
   #:set-flanger-filter-params
   ;; DC Removal Filter
   #:dc-removal-filter
   #:destroy-dc-removal-filter
   #:create-dc-removal-filter
   #:set-dc-removal-filter-params
   #:set-dc-removal-filter-params*
   ;; OpenMPT
   #:openmpt
   #:destroy-openmpt
   #:create-openmpt
   #:load-openmpt
   #:load-openmpt-mem
   #:load-openmpt-mem*
   #:load-openmpt-file
   #:set-openmpt-volume
   #:set-openmpt-looping
   #:set-openmpt-3d-min-max-distance
   #:set-openmpt-3d-attenuation
   #:set-openmpt-3d-doppler-factor
   #:set-openmpt-3d-processing
   #:set-openmpt-3d-listener-relative
   #:set-openmpt-3d-distance-delay
   #:set-openmpt-3d-collider
   #:set-openmpt-3d-collider*
   #:set-openmpt-3d-attenuator
   #:set-openmpt-3d-inaudible-behavior
   #:set-openmpt-filter
   #:stop-openmpt
   ;; Monotone
   #:defctype
   #:defcenum
   #:destroy-monotone
   #:create-monotone
   #:set-monotone-params
   #:set-monotone-params*
   #:load-monotone
   #:load-monotone-mem
   #:load-monotone-mem*
   #:load-monotone-file
   #:set-monotone-volume
   #:set-monotone-looping
   #:set-monotone-3d-min-max-distance
   #:set-monotone-3d-attenuation
   #:set-monotone-3d-doppler-factor
   #:set-monotone-3d-processing
   #:set-monotone-3d-listener-relative
   #:set-monotone-3d-distance-delay
   #:set-monotone-3d-collider
   #:set-monotone-3d-collider*
   #:set-monotone-3d-attenuator
   #:set-monotone-3d-inaudible-behavior
   #:set-monotone-filter
   #:stop-monotone
   ;; TedSid
   #:ted-sid
   #:destroy-ted-sid
   #:create-ted-sid
   #:load-ted-sid
   #:load-ted-sid-to-mem
   #:load-ted-sid-mem
   #:load-ted-sid-mem*
   #:load-ted-sid-file
   #:load-ted-sid-file-to-mem
   #:set-ted-sid-volume
   #:set-ted-sid-looping
   #:set-ted-sid-3d-min-max-distance
   #:set-ted-sid-3d-attenuation
   #:set-ted-sid-3d-doppler-factor
   #:set-ted-sid-3d-processing
   #:set-ted-sid-3d-listener-relative
   #:set-ted-sid-3d-distance-delay
   #:set-ted-sid-3d-collider
   #:set-ted-sid-3d-collider*
   #:set-ted-sid-3d-attenuator
   #:set-ted-sid-3d-inaudible-behavior
   #:set-ted-sid-filter
   #:stop-ted-sid))

(defpackage #:cl-soloud
  (:nicknames #:org.shirakumo.fraf.soloud)
  (:use #:cl #:cffi)
  (:shadow #:load)
  ;; filter.lisp
  (:export
   #:set-parameters
   #:biquad-resonant-filter
   #:echo-filter
   #:lofi-filter
   #:flanger-filter
   #:dc-removal-filter
   #:fft-filter
   #:bass-boost-filter)
  ;; soloud.lisp
  (:export
   #:c-backed-object
   #:handle
   #:soloud
   #:source
   #:filter
   #:collider
   #:attenuator
   #:playback
   #:soloud
   #:backend
   #:channel-location
   #:play
   #:seek
   #:volume
   #:pan
   #:set-absolute-pan
   #:relative-speed
   #:paused
   #:stop
   #:looping
   #:protected
   #:max-active-playback
   #:sample-rate
   #:oscillate-volume
   #:oscillate-pan
   #:oscillate-relative-speed
   #:sound-speed
   #:location
   #:direction
   #:up
   #:velocity
   #:min-max-distance
   #:attenuation
   #:doppler-factor
   #:filter-parameter
   #:group
   #:add)
  ;; source.lisp
  (:export
   #:volume
   #:looping
   #:min-max-distance
   #:attenuation
   #:doppler-factor
   #:3d-processing
   #:listener-relative
   #:distance-delay
   #:collider
   #:attenuator
   #:inaudible-behavior
   #:filter
   #:stop
   #:load
   #:load-mem
   #:wav-source
   #:wav-stream-source
   #:speech-source
   #:sfxr-source
   #:monotone-source
   #:ted-sid-source))

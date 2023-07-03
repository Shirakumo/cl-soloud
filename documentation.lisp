(in-package #:org.shirakumo.fraf.soloud)

;; bus.lisp
(docs:define-docs
  (type bus
    "The mixing busses are a special case of an audio stream.

They are a kind of audio stream that plays other audio streams.
Mixing bus can also play other mixing busses. Like any other
audio stream, mixing bus has volume, panning and filters. Only
one instance of a mixing bus can play at the same time, however;
trying to play the same bus several times stops the earlier
instance.

While a mixing bus doesn't generate audio by itself, playing
it counts against the maximum number of concurrent streams.

Mixing busses are protected by default (i.e, won't stop playing
if maximum number of concurrent streams is reached), and also
marked as \"must tick\" (i.e, will always be present in the
active voices list).

See SOURCE
See SOLOUD
See PLAY")

  (function soloud
    "Returns the base soloud object that this object relates back to.

See BUS
See PLAYBACK"))

;; filter.lisp
(docs:define-docs
  (type filter
    "Filters can be used to modify the sound some way.

Typical uses for a filter are to create environmental effects,
like echo, or to modify the way the speech synthesizer sounds
like.

See S-BACKED-OBJECT
See SET-PARAMETERS")

  (function set-parameters
    "Allows you to set the configuration parameters of the filter instance.

See FILTER")

  (type biquad-resonant-filter
    "The biquad resonant filter is a surprisingly cheap way to implement low and high pass filters, as well as some kind of band bass filter.

The implementation in SoLoud is based on
\"Using the Biquad Resonant Filter\",
Phil Burk, Game Programming Gems 3, p. 606.

The filter has three parameters - sample rate, cutoff frequency
and resonance. These can also be adjusted on live streams, for
instance to fade the low pass filter cutoff frequency for an
outdoors/indoors transition effect.

Currently, four parameters can be adjusted:
WET         --- Filter's wet signal; 1.0f for fully filtered,
                0.0f for original, 0.5f for half and half.
SAMPLERATE  --- Filter's samplerate parameter
FREQUENCY   --- Filter's cutoff frequency
RESONANCE   --- Filter's resonance - higher means sharper cutoff

See FILTER")

  (type echo-filter
    "The echo filter in SoLoud is a very simple one.

When the sound starts to play, the echo filter allocates a
buffer to contain the echo samples, and loops through this
until the sound ends.

The filter does not support changing of parameters on the fly,
nor does it take changing of relative play speed into account.

There are two parameters - DELAY and DECAY. Delay is the time
in seconds until the echo, and decay is multiplier for the echo.
If the multiplier is outside the [0..1[ range, the results are
unpredictable.

See FILTER")

  (type lofi-filter
    "The lofi filter is a signal degrading filter.

You can adjust both the bit depth and the sample rate of the
output, and these parameters can also be adjusted (and even faded)
on the fly.

The following parameters exist:
WET         --- Filter's wet signal; 1.0f for fully filtered,
                0.0f for original, 0.5f for half and half.
SAMPLERATE  --- Filter's samplerate parameter
BITDEPTH    --- Filter's bit-depth parameter

See FILTER")

  (type flanger-filter
    "The flanger filter can be used to create a \"flanger\" effect on the signal.

Applying this on a human voice may sound more \"robotic\", for instance.

Currently, four parameters can be adjusted:
WET    --- Filter's wet signal; 1.0f for fully filtered,
           0.0f for original, 0.5f for half and half.
FREQ   --- Filter's frequency
DELAY  --- Filter's delay

See FILTER")

  (type dc-removal-filter
    "This filter tries to remove DC signal from the audio.

In other words, it tries to center the waveform around 0. This
can be useful if some of the input waveforms gets stuck on 
non-zero values for a long time.

The filter does not support changing of parameters on the fly,
nor does it take changing of relative play speed into account.

The DC removal is performed by calculating the average sample
value over a relatively long period of time, and subtracting
this from the output.

There is one parameter, LENGTH, of how long the averaging
buffer should be. The time is in seconds.

See FILTER")

  (type fft-filter
    "The FFT filter is a short-time Fourier transform filter which can be used as basis for FFT-based effects.

The base implementation does a simple tone downshifting.

The filter exists mainly to adjust the speech synthesizer's
voice in strange ways. It can also be used as basis for other
FFT-based filters.

See FILTER")

  (type bass-boost-filter
    "The bassboost filter is a proof of concept FFT filter.

It simply multiplies the first few bands of the FFT by the
boost value.

There is one parameter, BOOST, of how strong the boost effect
is.

See FILTER")

  (type virtual-filter
    "This is a class for your own filter classes.

Subclass this and implement the FILTER and FILTER-CHANNEL
methods to create a new kind of filter.

See FILTER")

  (function filter
    "The FILTER function is the main workhorse of a filter.

It gets a buffer of samples, channel count, samplerate and
current stream time, and is expected to overwrite the samples
with filtered ones.

If channel count is not one, the layout of the buffer is such
that the first channel's samples come first, followed by the
second channel's samples, etc.

For stereo this basically means: L{samples}R{samples}

The default implementation calls filterChannel for every
channel in the buffer.

See VIRTUAL-FILTER
See FILTER-CHANNEL")

  (function filter-channel
    "Most filters are simpler to write on a channel-by-channel basis, so that they only deal with mono samples.

In this case, you may want to use this function instead. The
default implementation of FILTER calls this for every channel
in the source.

See VIRTUAL-FILTER
See FILTER"))

;; mp3.lisp
(docs:define-docs
  (type mp3-source
    "This source uses mpg123 to play back an MP3 file.

See VIRTUAL-SOURCE
See LOAD-FILE
See FILE")
  
  (function file
    "Accessor to the CL-MPG123:FILE instance used to process the MP3 file.

See MP3-SOURCE"))

;; soloud.lisp
(docs:define-docs
  (type soloud
    "This object represents the base instance needed to play audio with the soloud library.

It might be possible, though questionable in reasoning, to
create multiple instances of this at the same time. However,
doing so is not directly supported and probably not a good
idea. Thus, just make sure only to keep a single instance
around.

See C-BACKED-OBJECT
See BACKEND
See CHANNEL-LOCATION
See PLAY
See VOLUME
See STOP
See MAX-ACTIVE-PLAYBACK-COUNT
See ACTIVE-PLAYBACK-COUNT
See PLAYBACK-COUNT
See SAMPLE-RATE
See OSCILLATE-VOLUME
See SOUND-SPEED
See LOCATION
See DIRECTION
See UP
See VELOCITY
See PLAYING")

  (type playback
    "A handle representing the playback of an audio source.

See HANDLE
See SOLOUD
See SOURCE
See PLAY
See SEEK
See VOLUME
See PAN
See SET-ABSOLUTE-PAN
See RELATIVE-SPEED
See PAUSED-P
See STOP
See LOOPING-P
See PROTECTED-P
See SAMPLE-RATE
See OSCILLATE-VOLUME
See OSCILLATE-PAN
See OSCILLATE-RELATIVE-SPEED
See LOCATION
See VELOCITY
See MIN-MAX-DISTANCE
See ATTENUATION
See DOPPLER-FACTOR
See FILTER-PARAMETER
See INAUDIBLE-BEHAVIOR")

  (function source
    "Accessor to the playback's audio source that is being played back.

See PLAYBACK")

  (function backend
    "Read out information about SoLoud's audio backend.

Returns five values:
- The backend's keyword ID
- The backend's name as a string
- The number of channels of the backend
- The sample rate of the backend
- The internal buffer size of the backend

See SOLOUD")

  (function channel-location
    "Accessor to the location of a channel in 3d space.

The vector is a list of three floats.
The channel must be a valid channel id.

See SOLOUD")

  (function play
    "Play back an audio source on the target playback bus.

Returned is a PLAYBACK handle that represents
the access to the current playback. Note that SoLoud
invalidates some playback handles as soon as the audio 
source has stopped playing. However, this library cannot
know about this and cannot invalidate the handle for you
automatically, which means you might end up with an
invalid handle.

The bus can be either SOLOUD or BUS.

If DELAY is given, the sound is only played back after
the specified amount of time has passed. Without the
delay, the source is always played back as soon as
possible.

If LOCATION is given, the source is played back at the
specified location in 3d space.

If PAUSED is T, the source is started but paused and
won't play back until you unpause it.

See PLAYBACK
See SOLOUD
See BUS
See SOURCE")

  (function seek
    "Seek to the specified absolute position in the source.

Note that not all audio sources support seeking, and
for those that do this operation is usually rather
heavy.

See PLAYBACK")

  (function volume
    "Accessor to the volume.

The setf method accepts an additional argument, FADE,
which will fade the volume to the specified amount in
the given time.

See PLAYBACK
See SOLOUD
See SOURCE")

  (function pan
    "Accessor to the left/right panning of the playback.

The setf method accepts an additional argument, FADE,
which will fade the pan to the specified amount in the
given time.

See PLAYBACK")

  (function set-absolute-pan
    "Set the absolute pan of all the possible speakers of the playback.

Currently SoLoud does not support surround sound, so this
does nothing.

See PLAYBACK")

  (function relative-speed
    "Accessor to the relative playback speed of the playback.

The setf method accepts an additional argument, FADE,
which will fade the speed to the specified amount in
the given time.

See PLAYBACK")

  (function paused-p
    "Accessor to whether the playback is currently paused.

See PLAYBACK")

  (function stop
    "Stops the playback.

If you pass in the SOLOUD object, playback of all
sources will be stopped.

See PLAYBACK
See SOLOUD")

  (function looping-p
    "Accessor to whether the playback should loop or not.

See PLAYBACK
See SOURCE")

  (function protected-p
    "Accessor to whether the playback is protected or not.

Normally, if you try to play more sounds than there are slots,
SoLoud will kill off the oldest playing sound to make room.
That sound will most likely be your background music. This can
be worked around by protecting the sound.

See PLAYBACK")

  (function max-active-playback-count
    "Accessor to the maximum amount of playbacks that can be heard simultaneously.

See ACTIVE-PLAYBACK-COUNT
See SOLOUD")

  (function active-playback-count
    "Returns the current number of active playbacks.

See MAX-ACTIVE-PLAYBACK-COUNT
See PLAYBACK-COUNT
See SOLOUD")

  (function playback-count
    "Returns the current total number of playbacks.

See ACTIVE-PLAYBACK-COUNT
See SOLOUD")

  (function sample-rate
    "Returns the sample rate used by the source or target.

See SOLOUD
See SOURCE")

  (function oscillate-volume
    "Constantly oscillate between two volumes in the given time.

See SOLOUD
See PLAYBACK")

  (function oscillate-pan
    "Constantly oscillate between the two pans in the given time.

See PLAYBACK")

  (function oscillate-relative-speed
    "Constantly oscillate between the two relative speeds in the given time.

See PLAYBACK")

  (function sound-speed
    "Accessor to the speed of sound in the system.

The speed of sound is used to calculate doppler effects in
addition to the distance delay. Since SoLoud has no knowledge of
the scale of your coordinates, you may need to adjust the speed
of sound for these effects to work correctly. The default value
is 343, which assumes that your world coordinates are in meters
 (where 1 unit is 1 meter), and that the environment is dry air
at around 20 degrees Celsius.

See SOLOUD")

  (function location
    "Accessor to the location of the object in 3d space.

For the SOLOUD object, this is the location of the listener.
For everything else it's the location of the audio source.

See SOLOUD
See PLAYBACK
See 3D-DATA")

  (function direction
    "Accessor to the direction in which the listener faces in 3d space.

See SOLOUD")

  (function up
    "Accessor to the UP vector for the listener in 3d space.

See SOLOUD")

  (function velocity
    "Accessor to the velocity of the object in 3d space.

For the SOLOUD object, this is the velocity of the listener.
For everything else it's the velocity of the audio source.

See SOLOUD
See PLAYBACK
See 3D-DATA")

  (function min-max-distance
    "Accessor to the minimum and maximum distances for the source to be audible.

See PLAYBACK
See SOURCE
See 3D-DATA")

  (function attenuation
    "Accessor to the attenuation model and rolloff factor of the source.

See PLAYBACK
See SOURCE
See 3D-DATA")

  (function doppler-factor
    "Accessor to the doppler factor of the source.

See PLAYBACK
See SOURCE
See 3D-DATA")

  (function filter-parameter
    "Accessor to a parameter of the filter.

See PLAYBACK
See FILTER")

  (function inaudible-behavior
    "Accessor to the inaudible behaviour of the playback.

A list of two values:
- Whether the source should be ticked even when  inaudible
- Whether the source should be killed once inaudible")

  (function playing
    "Returns true if there is something playing back right now

See SOLOUD
See ACTIVE-PLAYBACK-COUNT")

  (type group
    "This class represents a \"playback group\".

Groups allow you to bundle together multiple playback
handles and change their properties all at once, rather
than risking updates happening between changes to
individual sources.

See PLAYBACK
See ADD")

  (function add
    "Add the object to another object.

For groups that's a playback, for sources that's a filter.

See GROUP
See SOURCE"))

;; source.lisp
(docs:define-docs
  (function decode-audio-source-flags
    "Decodes the integer of OR-combined flags into a list of keywords in the audio-source-flags enum.

See ENCODE-AUDIO-SOURCE-FLAGS")

  (function encode-audio-source-flags
    "Encodes the list of keywords form the audio-source-flags enum to an OR-combined flags integer.

See DECODE-AUDIO-SOURCE-FLAGS")

  (type source
    "Superclass for all audio sources.

Audio sources represent potential playback sources.
Most sources can be played back multiple times
simultaneously.

Before a source can be played back, it must be loaded.
Depending on the source, this can be one of the
following load methods:

See LOAD-FILE
See LOAD-MEM
See LOAD-PRESET
See LOAD-TEXT

See FILTER-MAP
See ADD
See WITHDRAW
See VOLUME
See LOOPING-P
See MIN-MAX-DISTANCE
See ATTENUATION
See DOPPLER-FACTOR
See 3D-PROCESSED-P
See LISTENER-RELATIVE-P
See DISTANCE-DELAYED-P
See COLLIDER
See ATTENUATOR
See INAUDIBLE-BEHAVIOR
See FILTER
See STOP")

  (function filter-map
    "Accessor to the hash table associating filter IDs to filter instances.

See SOURCE
See FILTER")

  (function load-file
    "Load the audio source's contents from a file on disk.

See WAV-SOURCE
See WAV-STREAM-SOURCE
See SFXR-SOURCE
See MONOTONE-SOURCE
See TED-SID-SOURCE
See MP3-SOURCE")

  (function load-mem
    "Load the audio source's contents from a memory buffer.

See WAV-SOURCE
See WAV-STREAM-SOURCE
See SFXR-SOURCE
See MONOTONE-SOURCE
See TED-SID-SOURCE")

  (function withdraw
    "Remove the given filter from the source again.

See FILTER
See SOURCE")

  (type collider
    "Superclass for all audio colliders.

See COLLIDER
See VIRTUAL-COLLIDER")

  (type attenuator
    "Superclass for all audio attenuators.

See ATTENUATOR
See VIRTUAL-ATTENUATOR")

  (function define-internal-source
   "Define a new source class.

This will automatically create the appropriate methods
for all the common source operations.

See SOURCE")

  (function 3d-processed-p
    "Accessor to whether the source is processed as a 3d audio source.

See SOURCE")

  (function listener-relative-p
    "Accessor to whether the source is relative to the listener or not.

If a sound is listener-relative, the listener's coordinates
are assumed to be (0 0 0) in calculations.

See SOURCE")

  (function distance-delayed-p
    "Accessor to whether the sound's playback will be delayed by its distance.

Since speed of sound is way slower than speed of light, in
reality we might see an explosion before we hear it. Default
is disabled, as this may be seen as a glitch since most games
do not bother simulating this.

See SOURCE")

  (function collider
    "Accessor to the custom audio collider instance.

SoLoud expects the collider to be there until all instances
of the sound have stopped. The application is responsible for
cleaning up the collider. Several sound sources may use the
same collider.

See SOURCE
See COLLIDER")

  (function attenuator
    "Accessor to the custom audio attenuator instance.

SoLoud expects the attenuator to be there until all instances
of the sound have stopped. The application is responsible for
cleaning up the attenuator. Several sound sources may use the
same attenuator.

See SOURCE
See ATTENUATOR")

  (function filter
    "Accessor to add or remove a filter to the source.

See FILTER
See SOURCE")

  (type wav-source
    "The SoLoud::Wav class represents a wave sound effect.

The source files may be in 8 or 16 bit raw RIFF WAV files,
or compressed Ogg Vorbis files.

The sounds are loaded and converted to float samples, which
means that every second of a 44100Hz stereo sound takes about
350kB of memory. The good side is, after loading, these
samples are very lightweight, as their processing is mostly
just a memory copy.

For lengthy samples like background music, you may want to
use SoLoud::WavStream instead. The Wav is all about speed, and
always decodes the whole sample into memory at load time.

See SOURCE
See LOAD-FILE
See LOAD-MEM")

  (type wav-stream-source
    "The SoLoud::WavStream class represents a wave sound effect that is streamed off disk while it's playing.

The source files may be in 8 or 16 bit raw RIFF WAV files, or
compressed Ogg Vorbis files.

The sounds are loaded in pieces while they are playing, which
takes more processing power than playing samples from memory,
but they require much less memory.

For short or often used samples, you may want to use
SoLoud::Wav instead.

See SOURCE
See LOAD-FILE
See LOAD-MEM")

  (type speech-source
    "The SoLoud::Speech class implements a simple Klatt-style formant speech synthesizer.

It's barely legible, not really human-like, but it's free,
and it's here.

Adjusting the speech synthesizer's output with audio filters
should allow for various voices, which, along with subtitles,
will let you add voice to your games cheaply.

For more serious use, feel free to study the source code and
play with the various internal parameters, as well as apply
various filters to the sound.

See SOURCE
See LOAD-TEXT")

  (function load-text
    "Load the text into the speech source for playback.

See SPEECH-SOURCE")

  (type sfxr-source
    "The SoLoud::Sfxr is a retro sound effect synthesizer based on the original Sfxr by Tomas Pettersson.

The original sfxr tool was designed to easily generate
sound effects for Ludum Dare 48h games. SoLoud includes the
same engine built in, so you can (should you wish) make
every coin, explosion etc. sound different.

The Sfxr sound sources also include a pseudo-random number
generator which should probably be moved to more general use
at some point.

See SOURCE
See LOAD-FILE
See LOAD-MEM
See LOAD-PRESET")

  (function load-preset
    "Load a preset into the SFXR-Source.

The symbol should be one of:
- :COINT
- :LASER
- :EXPLOSION
- :POWERUP
- :HURT
- :JUMP
- :BLIP

See SFXR-SOURCE")

  (type monotone-source
    "The SoLoud::Monotone is a replayer for MONOTONE tracker songs.

MONOTONE is a pc-speaker tracker, available on GitHub at

https://github.com/MobyGamer/MONOTONE/

The SoLoud MONOTONE replayer can play MONOTONE v1 songs
 (only format available at the time of this writing). You can
pick the number of hardware playbacks used - typically the songs
are composed for a single playback (PC beeper).

The waveform used is square wave.

See SOURCE
See LOAD-FILE
See LOAD-MEM")

  (type ted-sid-source
    "The SoLoud::TedSid is a replayer for TED and SID soundchip register write dumps.

This is based on tedplay (c) 2012 Attila Grosz, used under
Unlicense http://unlicense.org/.

TED is the soundchip of the commodore plus/4, and SID is the
soundchip of the commodore 64.

The TED and SID songs are actually complete c64 or plus/4
programs, so in order to avoid running a complete c64 emulator
in an audio engine, we only simulate the soundchips at real time.

You can use the Soloud.getInfo() interface to query TED and SID
register values while the song is playing. The SID registers are
mapped to values 0-31 and the TED registers to 64-69.

See SOURCE
See LOAD-FILE
See LOAD-MEM")

  (type virtual-source
    "Base class for your own custom audio source implementations.

In order to create a custom source, simply subclass this and
implement the following methods:
See GET-AUDIO
See HAS-ENDED
See SEEK-TO
See REWIND
See GET-INFO

See SOURCE
See BASE-SAMPLERATE
See CHANNELS
See FLAGS
See SINGLE-INSTANCE-P
See INAUDIBLE-KILL-P
See INAUDIBLE-TICK-P")

  (function base-samplerate
    "Accessor to the base samplerate used for this virtual source.

The samples requested by GET-AUDIO should be in this rate.

See VIRTUAL-SOURCE
See GET-AUDIO")

  (function channels
    "Accessor to the number of channels used for this virtual source.

The samples requested by GET-AUDIO should respect this value.

See VIRTUAL-SOURCE
See GET-AUDIO")

  (function flags
    "Accessor to the list of active flags for the source.

See VIRTUAL-SOURCE")

  (function define-flag-accessor
    "Define a shorthand accessor for a specific flag.

See FLAGS")

  (function single-instance-p
    "Accessor to whether there can only be a single playback instance of the audio source at once.

See FLAGS
See VIRTUAL-AUDIO-SOURCE")

  (function inaudible-kill-p
    "Accessor to whether this audio source should be killed when it is inaudible.

See FLAGS
See VIRTUAL-AUDIO-SOURCE
See INAUDIBLE-BEHAVIOR")

  (function inaudible-tick-p
    "Accessor to whether this audio source should still be ticked while inaudible.

See FLAGS
See VIRTUAL-AUDIO-SOURCE
See INAUDIBLE-BEHAVIOR")

  (function get-audio
    "This method is called to generate audio from a source.

SoLoud requests samples from the sound instance using this
function. If the instance generates more than one channel
 (i.e, stereo sound), the expected sample data first has the
first channel samples, then second channel samples, etc.

So, for stereo this is: L{samples}R{samples}

The getAudio function is also responsible for handling looping,
if the audio source supports it. If the audio source runs out
of data, the rest of the buffer should be set to zero.

Each sample should be a single-float.

See VIRTUAL-SOURCE")

  (function has-ended
    "This method is called to determine if the source has ended.

After mixing, SoLoud asks all audio instances whether they
have ended, and if they have, it will free the object and free
the channel. Supporting looping will likely affect the 
implementation of this function.

See VIRTUAL-SOURCE")

  (function seek-to
    "This method is called when a seek request is made.

This method is optional. The base implementation will simply
request (and discard) samples from the sound source until the
desired position has been reached; for many sound sources, a
smarter way exists.

See VIRTUAL-SOURCE")

  (function rewind
    "This method is called when a complete rewind is necessary.

To enable the base implementation of seek to seek backwards
from the current play position, sound source may implement the
rewind function. In most cases the rewind is easier to
implement than actual smart seeking.

See VIRTUAL-SOURCE")

  (function get-info
    "This method is called when information is requested from the source.

This method is optional. You can provide the interface to let
the application query real-time information about your audio
source. This information may be channel volumes, register
values, or some other information of interest.

See VIRTUAL-SOURCE")

  (type 3d-data
    "Container class for data related to 3d playback.

See LOCATION
See VELOCITY
See MIN-MAX-DISTANCE
See ATTENUATION
See DOPPLER-FACTOR
See COLLIDER
See ATTENUATOR
See COLLIDER-DATA
See DOPPLER-VALUE
See VOLUME
See CHANNEL-VOLUME
See FLAGS")
 
  (function collider-data
    "Returns the internal data for the audio collider.

See 3D-DATA")

  (function doppler-value
    "Returns the internal data for the doppler effect.

See 3D-DATA")

  (function channel-volume
    "Returns the volume of a single channel, with the panning in effect.

See 3D-DATA")

  (type virtual-collider
    "Superclass to create your own, custom colliders.

3d sound sources may have custom audio colliders attached
to them. By default, audio sources are only defined by
their position and maximum range, which makes the sound
sources point sources and omnidirectional.

With custom colliders, audio sources may be made to be
bound to some area, as well as be directional.

You must implement the COLLIDE method.

See COLLIDER
See COLLIDE")

  (function collide
    "Method called to compute the collision.

The return value is expected to be in the [0,1] range, and
gives the general volume level. The Soloud object and 3D-data
instance are given for convenience. Additionally, when
setting the collider, the application may also set a user data
integer value which is also provided to the custom collider
through this call.

The custom colliders are called while processing the 3d audio
in the update3dAudio() call, before any panning or attenuation
is calculated. Thus, if the COLLIDE function adjusts the
audio instance's 3d position, the changes will take effect.

For example, if a river collider was to be created, the
collider would check the player's distance to the river, and
adjust the sound source's 3d position to the point closest to
the player so that if the player runs along the river, the
sound would be heard from the direction of the river
 (instead of, for instance, from just the middle of the river).

Note that calling any SoLoud functions (even to set the
position of a 3d audio source) from the collide function will
most likely cause the application - or at least the audio
thread - to freeze due to mutex locks.

See VIRTUAL-COLLIDER")

  (type virtual-attenuator
    "Superclass to create your own, custom attenuators.

You must implement the ATTENUATE method.")

  (function attenuate
    "Method called to compute the attenuation.

The return value is expected to be in the [0,1 range, and
gives the general volume level.

The custom attenuators are called while processing the 3d
audio in the update3dAudio() call, before any panning is
calculated.

Note that calling any SoLoud functions (even to set the
position of a 3d audio source) from the attenuate function will
most likely cause the application - or at least the audio thread
 - to freeze due to mutex locks.

See VIRTUAL-ATTENUATOR"))

;; toolkit.lisp
(docs:define-docs
  (variable *c-object-table*
    "Table associating pointer addresses to their lisp-side objects.

See POINTER->OBJECT
See C-BACKED-OBJECT")

  (type c-backed-object
    "Superclass for all objects that track a C-allocated resource.

The class takes care of automatically freeing the resource
when an instance is being garbage collected, and allows you to
retrieve the object based on a pointer address.

See *C-OBJECT-TABLE*
See HANDLE
See CREATE-HANDLE
See DESTROY-HANDLE
See FREE")

  (function handle
    "Accessor to the pointer to the C object that this instance tracks.

See C-BACKED-OBJECT")

  (function pointer->object
    "Attempts to translate a pointer or pointer address of a C resource to its corresponding lisp object.

See *C-OBJECT-TABLE*
See C-BACKED-OBJECT")

  (function free
    "Explicitly and immediately frees the C object that this instance tracks.

See C-BACKED-OBJECT")

  (function with-callback-handling
    "Evaluates the body with the proper callback handling in place.

This also resolves the instance to a lisp instance if it can.
If the instance cannot be resolved, the body is not evaluated
and the default is returned instead.

If an error occurs, a message is printed to *standard-output*
and the error value is returned instead.

See POINTER->OBJECT")

  (function find-cffi-symbol
    "Attempt to find an equivalent symbol in the soloud CFFI package.

Replaces underscores in the temp symbol by the fill string."))

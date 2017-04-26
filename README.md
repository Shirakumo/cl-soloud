## About cl-soloud
This is a bindings library to [SoLoud](http://sol.gfxile.net/soloud/) which allows easy cross-platform mixing and playback.

## How To
Precompiled versions of the underlying library are included in this. If you want to build it manually however, refer to the [SoLoud](https://github.com/Shirakumo/soloud) repository.

Load the system through ASDF or Quicklisp:

    (ql:quickload :cl-soloud)

First you'll need to create an instance of the basic SoLoud object that will keep the system state around and allow you to manage playback.

    (defvar *s* (make-instance 'cl-soloud:soloud))

Next you'll want to get an audio source to play back from. The simplest would be a speech source, which crudely turns text to speech.

    (let ((source (make-instance 'cl-soloud:speech-source)))
      (cl-soloud:load-text source "This text to speech is pretty terrible")
      (cl-soloud:play source *s*))

Returned by `play` is a playback handle with which you can control the playback. Note that all the objects returned by this API will take care of automatically cleaning up the underlying resources when they are finalized. This means that you must keep a reference to your SoLoud object and sources around or things will randomly stop working.

SoLoud has a limit of how many playbacks can be played simultaneously. It is initially set to 16, but if you need more active sources then you can up it with the `:max-active-playback-count` initarg. Just note that the higher the number is, the more processing power will be required. If you play more things than the simultaneous limit, SoLoud will only play back the loudest ones. SoLoud also has a hard-coded maximum number of playbacks that can exist (this includes silent and active ones), which is set to 1024. The wrapper's API will make sure to check the maximum limit when you play a source.

## Extending SoLoud
SoLoud can be extended by adding custom audio sources, filters, attenuators, or colliders. All of those have a corresponding "virtual" class that you should subclass (`virtual-source` `virtual-filter` `virtual-attenuator` `virtual-collider`) and specialise their respective methods on. The explanation of which methods you need is described in the class' docstring, and what each method should do should be clearly outlined in its own docstring.

htwist
================

**htwist** is a work-in-progress Haskell library for dealing with the input/output/state
of a MIDI control surface.

It uses a custom MIDI library (`HTwist.Listener`) built on top of `hmidi` that allows for
the creation of 'Listener' functions that are called when receiving the specified MIDI messages,
similar to SuperCollider's `MIDIFunc` objects.

There are also type classes for converting arbitrary types (such as "Color" or "Brightness") into
the corresponding MIDI messages as specified by the MIDI controller.

The listener library and features can be used to control any MIDI controller, however this repository
currently includes the implementation for the MIDI Fighter Twister. In future work, I will be separating out the
library and including the MIDI Fighter Twister implementation as an example or separate repository and
publishing the htwist library on Hackage.

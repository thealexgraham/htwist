htwist
================

**htwist** is a Haskell project made to handle all aspects of MIDI Fighter Twister, including
knob color, pages, and all other input and output

It uses a custom MIDI library (`HTwist.Listener`) built on top of 'System.MIDI' that allows for
the creation of 'Listener' functions that are called when receiving the specified MIDI messages,
similar to SuperCollider's `MIDIFunc` objects.

There are also type classes for converting arbitrary types (such as "Color" or "Brightness" into
the corresponding MIDI messages as specified by the MIDI controller.

The listener library and features can be used to control any MIDI controller.

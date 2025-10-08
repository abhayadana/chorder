# Chorder
A chord instrument with assignable modifiers for Monome Norns.

-   Diatonic chords on white keys (I–VII), in any key/scale.
-   Voicings:
    -   None, Drop-2, Drop-3, Drop-2&4, Drop-1, Open position, Wide spread, Quartal, Quintal, Nearest voice leading, Smooth inversion
-   Timing & feel: global-clock aware quantize, strum (per-division steps), swing (grid/% modes), humanize (time/velocity)
-   Output:
    -   [mx.samples](https://github.com/schollz/mx.samples), MIDI, or both
    -   [nb voices](https://llllllll.co/t/n-b-et-al-v0-1/60374/156?u=modularbeat) work via [nbout](https://github.com/sixolet/nbout)
-   HUD & banner: active chord name

## Required
-   Monome Norns or equivalent
-   [mx.samples](https://github.com/schollz/mx.samples) engine & instrument packs installed at:
/home/we/dust/audio/mx.samples/<PackName> with valid manifest.lua per pack.
-   MIDI keyboard/controller (or Grid) for input & output

## Contributing
PRs & issues welcome!
Please include:

-   Repro steps (key/scale, voicing, modifiers used)
-   Norns version, mx.samples version, external MIDI device (if any)
-   Console logs from Maiden

## Acknowledgements
-   [monome](https://monome.org/) and the [lines community(https://llllllll.co/)]
-   [infinite digits](https://github.com/schollz) for the amazing mx.samples! (I also borrowed ideas for mx.samples integration from [o-o-o](https://github.com/schollz/o-o-o) and [awake-mx.samples](https://github.com/tomwaters/awake-mx.samples).)


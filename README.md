# Chorder
A chord instrument with assignable modifiers for Monome Norns.

-   Diatonic chords on white keys (I–VII), in any key/scale.
-   Voicings:
    -   None
    -   Drop-2: second note from top down an octave
    -   Drop-3: third note from top down an octave
    -   Drop-2&4: 2nd and 4th from top down an octave
    -   Drop-1: Top note down one octave
    -   Open position: Keep at least a 5th between any adjacent notes
    -   Wide spread: Move 3rd and 5th up an octave
    -   Quartal: Rebuild chord in stacked 4ths from root
    -   Quintal: Rebuild chord in stacked 5ths from root
    -   Nearest voice leading: Each note moves by ≤ interval threshold (e.g., ≤5 semitones) to next chord
    -   Smooth inversion: Auto-select inversion minimizing total motion from previous
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


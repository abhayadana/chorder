# Chorder
A four-voice chord-based instrument for Monome Norns.

- Lanes / voices
   - Three performance lanes: **Chord** (main), **Free Play A**, and **Free Play B**
   - MIDI in currently supports only one input device but each lane can be targeted by a different channel
   - On **Chord** lane, white keys play diatonic chords (I-VII) in globally assigned key/scale. Black keys are used for momentary modifiers (assignable in parameters): 7th, 9th, sus2, sus4, or solo
   - **Free Play** lanes can play chords or solo
      - **Free Play** key input mode can be either: white keys (diatonic degrees), all keys - quantized to scale, or all keys chromatic (solo only) 
   - The **Arp** lane receives chord information from the **Chord** lane.
   - Each output lane can target [mx.samples](https://github.com/schollz/mx.samples), MIDI out, or both
   - [nb voices](https://llllllll.co/t/n-b-et-al-v0-1/60374/156?u=modularbeat) work via [nbout](https://github.com/sixolet/nbout)
- Chord engine
   - Triads, 7th, 9th, sus2, sus4, inversions
   - Voicings:
      - None
      - Drop-2: second note from top down an octave
      - Drop-3: third note from top down an octave
      - Drop-2&4: 2nd and 4th from top down an octave
      - Drop-1: Top note down one octave
      - Open position: Keep at least a 5th between any adjacent notes
      - Wide spread: Move 3rd and 5th up an octave
      - Quartal: Rebuild chord in stacked 4ths from root
      - Quintal: Rebuild chord in stacked 5ths from root
      - Nearest voice leading: Each note moves by ≤ interval threshold (e.g., ≤5 semitones) to next chord
      - Smooth inversion: Auto-select inversion minimizing total motion from previous
- Timing & feel
   - Global-clock aware quantize
   - Swing (grid/% modes)
   - Humanize (time/velocity)
   - Strum shapes:
      - Up: Straight low-to-high sweep
      - Down: Straight high-to-low sweep
      - Up/down: Up then down bounce back without repeating
      - Down/up: Down then up bounce back without repeating
      - Random: Random permutation
      - Center-out: Start at median note (if even, start at lower-middle) then alternate outward
      - Outside-in: alternate the edges inward
      - Bass-bounce: Keep returning to lowest note
      - Treble-bounce: Mirror of bass-bounce from the highest note
      - Random (no repeat first): Random order each cycle, but forbid immediate repeats of the previous cycle’s first note (also no back-to-back duplicates within the cycle)
      - Random (stable ends): Anchor first and last hits, randomize only the middle notes
      - Edge-kiss: Rapid outer-pair tap with tiny rebound inward, biased to keep returning to the edges
      - Ping-pair: Move in adjacent pairs with a serpentine: (1,2), (3,4), … (then) (N,N-1), (N-2,N-3)…; each pair is lightly rolled
      - Arp-chunk 2-3: Ascend in 2-note then 3-note chunks, repeating that 2/3 cadence: 1,2 | 3,4,5 | 6,7 | 8,9,10 ... (wrap if N < 5)
      - Guitar-rake: Fast low→high rake with slight decel and velocity falloff (classic strummed guitar upstroke vibe)
      - Harp-gliss split: Play the lower half quickly upwards, then the upper half quickly upwards
      - Arp-chunk 2-3 (down): Descending version of 2-3 chunks
      - Guitar-rake (down): High→low rake with slight decel and velocity falloff (downstroke)
      - Harp-gliss interleaved: Alternate halves like two hands trading, but both halves move inward each time (smoother than outside-in due to gliss timing)
      - Bass-random: Pick random notes but with a strong bias to hit the lowest note frequently (acts like a bass ostinato with flurries above)
      - Top-random: Random with a strong bias toward the highest note (sparkly top with occasional dips)
      - Outer random → mid: Start with outer notes chosen at random, then progressively bias toward the center; probability shifts from edges → middle across the phrase
      - Weave lo→hi: A rising serpentine: 1,2,1,3,2,4,3,5 ... (small backsteps while generally climbing)
      - Weave hi→lo: Mirror: N,N-1,N,N-2,N-1,N-3 ... (small backsteps while generally falling)
      - Inside-out (alt): Start near center; strictly alternate sides as you expand outward
      - Inside-out (random): Inside-out expansion but choose left/right randomly at each outward step
      - Odds then evens: Hit odd indices low→high, then even indices low→high: 1,3,5,..., 2,4,6,...
      - Evens then odds: The inverse ordering of Odds then evens
      - Low-half up / high-half down: Split at median: play lower half ascending, then upper half descending
- Arppegiator (pattern library)
   - Patterns include basic (same as strum shapes above from Up to Treble-bounce) and "genre-specific" (according to ChatGPT)
   - Triggering modes: key-held and free-run
   - Timing divisions include triplets and dotted
   - Per-step probability
   - Ratchets and ratchet probability

## Required
- Monome Norns or equivalent
- [mx.samples](https://github.com/schollz/mx.samples) engine & instrument packs installed at
/home/we/dust/audio/mx.samples/<PackName>
- MIDI keyboard/controller (or Grid) for input & output. Chorder was developed and tested using a Novation Launchkey Mini MK3

## Contributing
Pull requests & issues welcome!

## Acknowledgements
- [monome](https://monome.org/) and the [lines community](https://llllllll.co/)
- [infinite digits](https://github.com/schollz) for the amazing mx.samples! (I also borrowed ideas for mx.samples integration from [o-o-o](https://github.com/schollz/o-o-o) and [awake-mx.samples](https://github.com/tomwaters/awake-mx.samples).)
- [modular beat](https://llllllll.co/u/modularbeat) for [dreamsequence](https://github.com/dstroud/dreamsequence). I borrowed lots of ideas from here too!
- ChatGPT/Copilot for assistance with refactoring, patternlib, etc.

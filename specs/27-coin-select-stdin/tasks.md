# Tasks: text stdin coin selection executable

## Slice 1: stdin text executable and wasm proof

- [X] T027-S1 Add a focused RED by observing `exe:coin-select` is missing before implementation.
- [X] T027-S1 Add `coin-select` text stdin parsing, deterministic `NonRandom` selection, and deterministic text output.
- [X] T027-S1 Wire cabal metadata without adding JSON dependencies for native and wasm builds.
- [X] T027-S1 Add a lowercase `just ci` alias while preserving the existing `CI` recipe.
- [X] T027-S1 Extend the GitHub wasm job to build/run `coin-select` and compare native/wasm output for the sample text.
- [X] T027-S1 Run focused native proof, wasm proof, and `./gate.sh`.
- [X] T027-S1 Commit with subject `feat: add text stdin coin selection executable` and trailer `Tasks: T027-S1`.

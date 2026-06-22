# Tasks: JSON stdin coin selection executable

## Slice 1: stdin JSON executable and wasm proof

- [ ] T027-S1 Add a focused RED by observing `exe:coin-select` is missing before implementation.
- [ ] T027-S1 Add `coin-select` JSON stdin parsing, deterministic `NonRandom` selection, and compact deterministic output.
- [ ] T027-S1 Wire cabal metadata and executable-only JSON dependencies for native and wasm builds.
- [ ] T027-S1 Add a lowercase `just ci` alias while preserving the existing `CI` recipe.
- [ ] T027-S1 Extend the GitHub wasm job to build/run `coin-select` and compare native/wasm output for the sample JSON.
- [ ] T027-S1 Run focused native proof, wasm proof, and `./gate.sh`.
- [ ] T027-S1 Commit with subject `feat: add JSON stdin coin selection executable` and trailer `Tasks: T027-S1`.

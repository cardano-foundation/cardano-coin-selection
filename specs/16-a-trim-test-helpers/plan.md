# Plan

## Scope

Split the existing test-helper surface into a named `gens` sublibrary while
leaving the production library API and source files otherwise unchanged. This
ticket is a cabal hygiene refactor: the main library gets a leaner dependency
surface, and the test suite opts into the helper component explicitly.

## Verified Facts

- The current main library exposes 12 helper modules whose names are `.Gen`,
  `Gen.Extra`, or `Test.Laws`.
- Those helper modules are imported by tests and by other helper modules.
- No core production module currently imports the helper modules.
- Baseline `./gate.sh` passes under `nix develop --quiet -c just CI` with 302
  examples and 0 failures.
- The repository recipe is `just CI`; the issue text's `just ci` spelling is
  not an available just recipe in this checkout.

## Slice 1: `gens` Sublibrary Boundary

Owned files:

- `cardano-coin-selection.cabal`

Forbidden scope:

- Do not edit files under `lib/`.
- Do not edit files under `test/`.
- Do not edit `app/`.
- Do not edit `cabal.project`, `cabal-wasm.project`, flake, Nix, CI, fixed
  hashes, or dependency pins.
- Do not edit specs, tasks, `gate.sh`, or PR metadata.

Work:

- Add a named `library gens` stanza using `hs-source-dirs: lib`.
- Expose only the helper modules listed in the specification from `library
  gens`.
- Make `library gens` depend on the main library plus the helper-module
  dependencies it needs, including the test-framework dependencies currently
  polluting the main library.
- Remove the helper modules from the main library's `exposed-modules`.
- Remove `hspec`, `QuickCheck`, `quickcheck-classes`, and `pretty-simple` from
  the main library's `build-depends`.
- Add `cardano-coin-selection:gens` to the `unit` test-suite
  `build-depends`.

Proof:

- RED: inspect the starting cabal file and record that the main library exposes
  helper modules and lists the four test-framework dependencies.
- GREEN:
  - `./gate.sh`
  - a public main-library dependency check showing the four test-framework
    dependencies are absent from the main `library` stanza.
- Optional, if the toolchain is available without blocking the ticket:
  `wasm32-wasi-cabal --project-file=cabal-wasm.project build
  lib:cardano-coin-selection`.

Commit:

- `refactor: move generator helpers into gens sublibrary`
- body trailer: `Tasks: T016`


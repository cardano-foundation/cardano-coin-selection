# Plan

## Scope

Split the existing test-helper surface into a named `gens` sublibrary backed by
a non-overlapping `test/gens` source root. The production library API and core
module source files remain unchanged; the main library gets a leaner dependency
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
- A cabal-only split using `hs-source-dirs: lib` for `library gens` is not
  viable: the helper component sees production modules as home modules and
  creates duplicate type identities in the test suite. The helper modules must
  move to a non-overlapping source root.

## Slice 1: `gens` Sublibrary Boundary

Owned files:

- `cardano-coin-selection.cabal`
- `lib/Cardano/CoinSelection/Balance/Gen.hs` -> `test/gens/Cardano/CoinSelection/Balance/Gen.hs`
- `lib/Cardano/CoinSelection/Gen/Extra.hs` -> `test/gens/Cardano/CoinSelection/Gen/Extra.hs`
- `lib/Cardano/CoinSelection/Test/Laws.hs` -> `test/gens/Cardano/CoinSelection/Test/Laws.hs`
- `lib/Cardano/CoinSelection/Types/AssetId/Gen.hs` -> `test/gens/Cardano/CoinSelection/Types/AssetId/Gen.hs`
- `lib/Cardano/CoinSelection/Types/AssetName/Gen.hs` -> `test/gens/Cardano/CoinSelection/Types/AssetName/Gen.hs`
- `lib/Cardano/CoinSelection/Types/Coin/Gen.hs` -> `test/gens/Cardano/CoinSelection/Types/Coin/Gen.hs`
- `lib/Cardano/CoinSelection/Types/TokenBundle/Gen.hs` -> `test/gens/Cardano/CoinSelection/Types/TokenBundle/Gen.hs`
- `lib/Cardano/CoinSelection/Types/TokenMap/Gen.hs` -> `test/gens/Cardano/CoinSelection/Types/TokenMap/Gen.hs`
- `lib/Cardano/CoinSelection/Types/TokenPolicyId/Gen.hs` -> `test/gens/Cardano/CoinSelection/Types/TokenPolicyId/Gen.hs`
- `lib/Cardano/CoinSelection/Types/TokenQuantity/Gen.hs` -> `test/gens/Cardano/CoinSelection/Types/TokenQuantity/Gen.hs`
- `lib/Cardano/CoinSelection/UTxOIndex/Gen.hs` -> `test/gens/Cardano/CoinSelection/UTxOIndex/Gen.hs`
- `lib/Cardano/CoinSelection/UTxOSelection/Gen.hs` -> `test/gens/Cardano/CoinSelection/UTxOSelection/Gen.hs`

Forbidden scope:

- Do not edit core production files under `lib/`.
- Do not edit test specs under `test/spec/`.
- Do not edit `app/`.
- Do not edit `cabal.project`, `cabal-wasm.project`, flake, Nix, CI, fixed
  hashes, or dependency pins.
- Do not edit specs, tasks, `gate.sh`, or PR metadata.

Work:

- Move only the helper modules listed above from `lib/` to `test/gens/` with
  their module names unchanged.
- Add a named `library gens` stanza using `hs-source-dirs: test/gens`.
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
  - `git status --short` / `git diff --stat` showing only cabal plus helper
    module moves, with no core production source edits.
- Optional, if the toolchain is available without blocking the ticket:
  `wasm32-wasi-cabal --project-file=cabal-wasm.project build
  lib:cardano-coin-selection`.

Commit:

- `refactor: move generator helpers into gens sublibrary`
- body trailer: `Tasks: T016`

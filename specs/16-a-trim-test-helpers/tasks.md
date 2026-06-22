# Tasks

## Slice 1: `gens` Sublibrary Boundary

- [X] T016 Add a `library gens` component for the generator and test-law
  helper modules under `test/gens`.
- [X] T016 Move only the generator/test-law helper modules from `lib/` to
  `test/gens/`, preserving module names.
- [X] T016 Remove generator/test-law helper modules from the main library
  exposed module list.
- [X] T016 Remove `hspec`, `QuickCheck`, `quickcheck-classes`, and
  `pretty-simple` from the main library `build-depends`.
- [X] T016 Add `cardano-coin-selection:gens` to the unit test-suite
  dependencies.
- [X] T016 Prove the refactor with `./gate.sh` and a main-library dependency
  check.
- [X] T016 Commit with subject
  `refactor: move generator helpers into gens sublibrary` and trailer
  `Tasks: T016`.

# Tasks

## Slice 1: `gens` Sublibrary Boundary

- [ ] T016 Add a `library gens` component for the generator and test-law
  helper modules under `test/gens`.
- [ ] T016 Move only the generator/test-law helper modules from `lib/` to
  `test/gens/`, preserving module names.
- [ ] T016 Remove generator/test-law helper modules from the main library
  exposed module list.
- [ ] T016 Remove `hspec`, `QuickCheck`, `quickcheck-classes`, and
  `pretty-simple` from the main library `build-depends`.
- [ ] T016 Add `cardano-coin-selection:gens` to the unit test-suite
  dependencies.
- [ ] T016 Prove the refactor with `./gate.sh` and a main-library dependency
  check.
- [ ] T016 Commit with subject
  `refactor: move generator helpers into gens sublibrary` and trailer
  `Tasks: T016`.

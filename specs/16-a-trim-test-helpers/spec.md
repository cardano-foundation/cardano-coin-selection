# Specification

## User Story

As a downstream user of `cardano-coin-selection`, I want the main library
component to avoid test-framework dependencies, so depending on the production
coin-selection API does not pull Hspec, QuickCheck, quickcheck-classes, or
pretty-simple into my build plan.

## Requirements

- Move generator and test-law helper modules out of the main library component
  and into a named internal sublibrary `cardano-coin-selection:gens`.
- Move only the helper module source files out of the main library source root
  into `test/gens/`, so `library gens` has a non-overlapping source root and
  imports production modules from the main library component.
- The main `library` component must no longer expose:
  - `Cardano.CoinSelection.Balance.Gen`
  - `Cardano.CoinSelection.Gen.Extra`
  - `Cardano.CoinSelection.Test.Laws`
  - `Cardano.CoinSelection.Types.AssetId.Gen`
  - `Cardano.CoinSelection.Types.AssetName.Gen`
  - `Cardano.CoinSelection.Types.Coin.Gen`
  - `Cardano.CoinSelection.Types.TokenBundle.Gen`
  - `Cardano.CoinSelection.Types.TokenMap.Gen`
  - `Cardano.CoinSelection.Types.TokenPolicyId.Gen`
  - `Cardano.CoinSelection.Types.TokenQuantity.Gen`
  - `Cardano.CoinSelection.UTxOIndex.Gen`
  - `Cardano.CoinSelection.UTxOSelection.Gen`
- The main `library` component must no longer list `hspec`, `QuickCheck`,
  `quickcheck-classes`, or `pretty-simple` in its `build-depends`.
- The `unit` test suite must depend on both the main library and
  `cardano-coin-selection:gens`, and tests must continue to import the same
  generator/test-law module names.
- Core production module source files must stay under `lib/` and must not be
  modified for this ticket.
- If a non-test, non-generator core module imports a `.Gen` module or
  `Cardano.CoinSelection.Test.Laws`, stop and ask through the parent Q/A
  protocol before widening scope.

## Acceptance

- `./gate.sh` passes. Baseline gate on this branch reported 302 examples and
  0 failures.
- A public library dependency check confirms the main library component does
  not depend on `hspec`, `QuickCheck`, `quickcheck-classes`, or
  `pretty-simple`.
- The branch remains compatible with the existing GitHub CI matrix, including
  build, docs, and wasm checks, before this PR is marked complete.

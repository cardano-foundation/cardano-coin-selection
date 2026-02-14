# WIP: Add tests to cardano-coin-selection

## Status
All 169 tests pass. Library fully builds with GHC 9.12.2.

## Plan
1. [x] Create worktree feat/tests
2. [x] Create QC Extra shim (chooseNatural, shrinkNatural, genSized2, genSized2With, shrinkInterleaved)
3. [x] Create Gen modules (7: AssetName, TokenPolicyId, Coin, TokenQuantity, AssetId, TokenMap, TokenBundle)
4. [x] Create Test.Laws utility (from wallet test-utils)
5. [x] Copy UTxOSelectionSpec (no changes needed)
6. [x] Copy + adapt CollateralSpec (wallet import renames)
7. [x] Copy + adapt UTxOIndexSpec (wallet import renames + Test.Utils.Laws)
8. [x] Update cabal file with all new modules + test suite
9. [x] Build and fix compilation errors
10. [x] Add equipartition/partition/unsafePartition back to Coin, TokenQuantity, TokenMap, TokenBundle
11. [x] Add equipartitionNatural to Internal.Numeric
12. [x] Copy NonRandom module
13. [x] Set up fourmolu.yaml, .hlint.yaml, justfile
14. [x] Format all sources with fourmolu
15. [x] Fix UTxOIndex/UTxOSelection generators (genFunction via promote, liftShrink2)
16. [x] All 169 tests pass
17. [ ] Add BalanceSpec (needs Fmt, Cardano.Numeric.Util, Test.Utils.Pretty, Test.QuickCheck.Extra — heavier adaptation)

## Key Decisions
- **Partition Gen functions removed** — `partitionDefault` was stripped from Coin/TokenQuantity; no test spec uses partition generators
- **equipartition restored** — Balance.hs uses Coin.unsafePartition, TokenBundle.equipartitionAssets, TokenBundle.equipartitionQuantitiesWithUpperBound; reimplemented using partitionNatural/equipartitionNatural in Internal.Numeric
- **QC.Extra inlined** — only 5 functions needed, avoids pulling in wallet test-utils
- **TokenPolicyId.Gen** — constructs Hash from bytes directly (no Data.Text.Class / fromText)
- **genFunction inlined** — uses `promote` from `Test.QuickCheck.Gen.Unsafe` directly
- **genericRoundRobinShrink replaced** — `liftShrink2` used instead (simpler, no generics-sop dependency for shrinking)
- **BalanceSpec deferred** — heaviest deps (Fmt, Cardano.Numeric.Util, Test.Utils.Pretty, Test.QuickCheck.Extra used directly)
- **Test runner** — plain hspec-discover (no Main.Utf8 / Test.Hspec.Extra)

## Files Created
- lib/Cardano/CoinSelection/Gen/Extra.hs
- lib/Cardano/CoinSelection/Test/Laws.hs
- lib/Cardano/CoinSelection/Types/{AssetName,TokenPolicyId,Coin,TokenQuantity,AssetId,TokenMap,TokenBundle}/Gen.hs
- lib/Control/Monad/Random/NonRandom.hs
- test/spec/Spec.hs
- test/spec/Cardano/CoinSelection/{CollateralSpec,UTxOIndexSpec,UTxOSelectionSpec}.hs
- fourmolu.yaml, .hlint.yaml, justfile

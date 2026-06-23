# WASM Portability Audit

## Result summary

For the public summary of the browser-runnable WASM build and hosted demo, see
[Browser WASM](../browser-wasm.md).

The library has no direct `foreign import` declarations under `lib/`.
Among the direct dependencies in the library stanza, `memory` is the only
known WASM-hostile dependency identified by this audit. The repository-local
use is `Data.ByteArray.ByteArrayAccess`, derived for `Hash` in
`Cardano.CoinSelection.Types.Hash`.

The planned mitigation belongs to issue #12: switch the `memory` use to `ram`
under `arch(wasm32)`. This page records the audit result only and does not
implement that change.

## Local evidence

Run these commands from the repository root:

```sh
rg "foreign import" lib
rg "memory|ByteArrayAccess|Data.ByteArray" lib cardano-coin-selection.cabal
```

The `foreign import` scan returns no matches. The dependency/use scan shows
`memory` in `cardano-coin-selection.cabal` and `Data.ByteArray.ByteArrayAccess`
in `lib/Cardano/CoinSelection/Types/Hash.hs`.

## Direct dependency matrix

| Dependency | Status | Repository-specific WASM mitigation |
|------------|--------|--------------------------------------|
| `base` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `bytestring` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `commutative-semigroups` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `containers` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `deepseq` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `exact-combinatorics` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `generic-lens` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `generics-sop` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `hashable` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `hspec` | Expected portable | None identified; pure Hackage dependency for this audit. See the public test-helper note below. |
| `int-cast` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `lattices` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `math-functions` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `memory` | Blocked for WASI | WASI lacks `mmap`. Local use is `Data.ByteArray.ByteArrayAccess` derived for `Hash` in `Types/Hash.hs`. Issue #12 owns switching to `ram` under `arch(wasm32)`. |
| `MonadRandom` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `monoid-subclasses` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `monoidmap` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `pretty-simple` | Expected portable | None identified; pure Hackage dependency for this audit. See the public test-helper note below. |
| `QuickCheck` | Expected portable | None identified; pure Hackage dependency for this audit. See the public test-helper note below. |
| `quickcheck-classes` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `quiet` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `random` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `safe` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `text` | Expected portable | None identified; pure Hackage dependency for this audit. |
| `transformers` | Expected portable | None identified; pure Hackage dependency for this audit. |

## Public test-helper dependencies

`hspec`, `QuickCheck`, and `pretty-simple` are public library dependencies
because generator modules and `Cardano.CoinSelection.Test.Laws` are exposed by
the library. Issue #16 tracks moving that test-helper surface out of the public
library. This audit does not make that cabal or module-surface change.

## Out of scope

This PR makes no cabal, Haskell source, Nix, or CI changes. The WASI mitigation
for `memory` is intentionally left to issue #12.

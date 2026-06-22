# Plan

## Scope

Remove the final `int-cast` blocker left by issue #13 so the existing WASM
Cabal project builds `lib:cardano-coin-selection` completely.

## Verified Facts

- Native baseline passed with `nix develop --quiet -c just CI`.
- The baseline WASM command fails in `int-cast-0.2.0.0` on:
  `Not in scope: type constructor or class 'HTYPE_SIG_ATOMIC_T'`.
- The Hackage tarball for `int-cast-0.2.0.0` ships `Data/IntCast.hs`
  directly, not an `.hsc` file. The missing symbol is a CPP macro consumed by
  shipped Haskell source after including `HsBaseConfig.h`.
- A temporary out-of-repo project file with:
  `package int-cast` / `ghc-options: -DHTYPE_SIG_ATOMIC_T=HTYPE_INT`
  built `int-cast` and then built all 30 library modules for
  `cardano-coin-selection` under `wasm32-wasi`.
- The repository's native CI recipe is `just CI`.

## Slice 1: Project-file `int-cast` WASM workaround

Owned files:

- `cabal-wasm.project`

Work:

- Add a `package int-cast` stanza to `cabal-wasm.project`.
- Set `ghc-options: -DHTYPE_SIG_ATOMIC_T=HTYPE_INT` for that package.
- Do not modify library source, tests, native `cabal.project`, dependency
  bounds, or the existing `paolino/ram` source pin.
- Run the full gate; the wasm command must succeed, not merely move to a new
  blocker.

Proof:

- RED: before editing, record the current `wasm32-wasi-cabal
  --project-file=cabal-wasm.project build lib:cardano-coin-selection` failure
  on `HTYPE_SIG_ATOMIC_T`.
- GREEN: `./gate.sh` succeeds, covering native CI and the full wasm library
  build.

Commit:

- `build: teach wasm project int-cast sig_atomic_t`
- body trailer: `Tasks: T020`

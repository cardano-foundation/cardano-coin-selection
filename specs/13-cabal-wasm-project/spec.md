# Issue #13: WASM Cabal Project

## User Story

As a maintainer preparing `cardano-coin-selection` for `wasm32-wasi`, I want a
dedicated Cabal project file that lets `ghc-wasm-meta` build the library with
WASI-compatible options while leaving native builds unchanged.

## Requirements

- Add `cabal-wasm.project` at the repository root.
- Match the native project package set and index state:
  `packages: .` and `index-state: 2025-10-01T00:00:00Z`.
- Allow the expected GHC 9.12 WASM bounds relaxations:
  `*:template-haskell`, `*:base`, `*:deepseq`, `*:ghc-prim`, `*:time`, and
  `*:text`.
- Configure `package ram` with WASI mmap emulation:
  `-optc-D_WASI_EMULATED_MMAN`, `-optl-lwasi-emulated-mman`,
  `--cflag=-D_WASI_EMULATED_MMAN`, and `--lflag=-lwasi-emulated-mman`.
- Disable tests in the WASM project with `tests: False`.
- If test-suite dependencies still pollute the WASM solve, add a manual
  `wasm` flag to `cardano-coin-selection.cabal` that makes the test-suite
  `buildable: False` only when the flag is enabled, and pass `-fwasm` from
  `cabal-wasm.project`.
- Keep native builds and the existing `just CI` recipe unaffected.

## Acceptance

- `nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12' --command bash -lc 'wasm32-wasi-cabal --project-file=cabal-wasm.project build lib:cardano-coin-selection'`
  succeeds.
- `nix develop --quiet -c just CI` succeeds after the WASM project changes.
- `./gate.sh` succeeds and includes both native CI and the WASM library build.

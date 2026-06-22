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
- Pin the patched `paolino/ram` fork at
  `e6d863d240246e0a1af3dd12cff7047f696f81ea` with a nix32 `--sha256` so
  `ram` builds past the WASI `memcpy`/`memset` linker mismatch.
- Disable tests in the WASM project with `tests: False`.
- Add a manual `wasm` flag to `cardano-coin-selection.cabal` that makes the
  test-suite `buildable: False` only when the flag is enabled, and pass
  `+wasm` from `cabal-wasm.project`.
- Keep native builds and the existing `just CI` recipe unaffected.
- Do not fix `int-cast`; that blocker belongs to issue #20.

## Acceptance

- `nix develop --quiet -c just CI` succeeds after the WASM project changes.
- `wasm32-wasi-cabal --project-file=cabal-wasm.project build ram` succeeds,
  proving the patched `ram` fork links with WASI.
- The library WASM build resolves and builds past `ram`, then fails only on
  `int-cast-0.2.0.0` with the known `HTYPE_SIG_ATOMIC_T` error delegated to
  issue #20.
- `./gate.sh` succeeds by checking native CI, the `ram` WASM build, and the
  documented `int-cast` blocker shape.

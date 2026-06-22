# Plan

## Scope

Implement the A3 deliverable for issue #13: a dedicated `cabal-wasm.project`
that cross-compiles the library with `ghc-wasm-meta` after #12's native/wasm
`memory` to `ram` split.

## Verified Facts

- `cabal.project` contains `packages: .` and
  `index-state: 2025-10-01T00:00:00Z`.
- The library already selects `ram >=0.22 && <0.23` under `arch(wasm32)`.
- `ram` needs a patched fork for WASI because upstream `0.22.0` imports
  `memcpy`/`memset` with `IO ()` return types that the WASM linker rejects.
- The remaining `int-cast` `HTYPE_SIG_ATOMIC_T` failure is owned by issue #20,
  not this ticket.
- The repo's native CI recipe is `just CI`.
- The PR-local `gate.sh` runs `git diff --check`,
  `nix develop --quiet -c just CI`, proves `ram` builds with
  `ghc-wasm-meta`, and accepts only the known `int-cast` blocker for #20.

## Slice 1: WASM Cabal Project

Owned files:

- `cabal-wasm.project`
- `cardano-coin-selection.cabal` for the manual `wasm` flag that disables the
  `unit` test-suite only when enabled by `cabal-wasm.project`.

Work:

- Add `cabal-wasm.project` with the required package list, index-state,
  `allow-newer` entries, patched `paolino/ram` source pin with nix32
  `--sha256`, `package ram` mmap-emulation options, and `tests: False`.
- Add a manual `wasm` flag to the cabal file, guard the `unit` test-suite with
  `if flag(wasm) buildable: False`, and pass `+wasm` in the WASM project.
- Run the relaxed WASM acceptance command through `./gate.sh`: `ram` must build
  successfully and the library build may fail only on `int-cast`
  `HTYPE_SIG_ATOMIC_T`.
- Keep native Cabal behavior unchanged.

Proof:

- RED: record the expected pre-change failure of the WASM command because
  `cabal-wasm.project` is absent or incomplete.
- GREEN: `./gate.sh`, which covers native CI, the patched `ram` WASM build,
  and the documented #20 `int-cast` blocker.

Commit:

- `build: add cabal wasm project`
- body trailer: `Tasks: T013`

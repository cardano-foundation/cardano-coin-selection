# Plan

## Scope

Implement the A3 deliverable for issue #13: a dedicated `cabal-wasm.project`
that cross-compiles the library with `ghc-wasm-meta` after #12's native/wasm
`memory` to `ram` split.

## Verified Facts

- `cabal.project` contains `packages: .` and
  `index-state: 2025-10-01T00:00:00Z`.
- The library already selects `ram >=0.22 && <0.23` under `arch(wasm32)`.
- The repo's native CI recipe is `just CI`.
- The PR-local `gate.sh` runs `git diff --check`,
  `nix develop --quiet -c just CI`, and the `ghc-wasm-meta` library build.

## Slice 1: WASM Cabal Project

Owned files:

- `cabal-wasm.project`
- `cardano-coin-selection.cabal` only if the WASM solver proves the optional
  `wasm` flag is necessary to exclude test-suite dependencies.

Work:

- Add `cabal-wasm.project` with the required package list, index-state,
  `allow-newer` entries, `package ram` mmap-emulation options, and
  `tests: False`.
- Run the WASM build acceptance command.
- If the WASM solve fails because test-suite dependencies are still in scope,
  add a manual `wasm` flag to the cabal file, guard the `unit` test-suite with
  `if flag(wasm) buildable: False`, and pass `-fwasm` in the WASM project.
- Keep native Cabal behavior unchanged.

Proof:

- RED: record the expected pre-change failure of the WASM command because
  `cabal-wasm.project` is absent or incomplete.
- GREEN: `./gate.sh`, which covers native CI and the WASM build.

Commit:

- `build: add cabal wasm project`
- body trailer: `Tasks: T013`

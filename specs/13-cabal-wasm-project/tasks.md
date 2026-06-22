# Tasks

## Slice 1: WASM Cabal Project

- [ ] T013 Add `cabal-wasm.project` with the native index-state, required
  `allow-newer` relaxations, patched `paolino/ram` source pin with nix32
  `--sha256`, `ram` mmap-emulation options, `+wasm`, and `tests: False`.
- [ ] T013 Add a manual `wasm` cabal flag that disables the `unit` test-suite
  only when enabled by `cabal-wasm.project`.
- [ ] T013 Run `./gate.sh` and record native CI, patched `ram` WASM build
  success, and the expected `int-cast` `HTYPE_SIG_ATOMIC_T` blocker for #20.
- [ ] T013 Commit with subject `build: add cabal wasm project` and trailer
  `Tasks: T013`.

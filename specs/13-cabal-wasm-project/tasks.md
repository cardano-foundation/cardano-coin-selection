# Tasks

## Slice 1: WASM Cabal Project

- [ ] T013 Add `cabal-wasm.project` with the native package/index-state,
  required `allow-newer` relaxations, `ram` mmap-emulation options, and
  `tests: False`.
- [ ] T013 If required by the WASM solve, add a manual `wasm` cabal flag that
  disables the `unit` test-suite only when enabled by `cabal-wasm.project`.
- [ ] T013 Run `./gate.sh` and record the native CI plus WASM build result.
- [ ] T013 Commit with subject `build: add cabal wasm project` and trailer
  `Tasks: T013`.

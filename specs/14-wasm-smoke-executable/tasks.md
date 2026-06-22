# Tasks

## Slice 1: Deterministic Smoke Executable

- [ ] T014 Add `executable coin-select-smoke` with minimal dependencies and
  native/wasm buildability.
- [ ] T014 Add `app/Main.hs` that constructs a valid deterministic selection
  problem, runs `performSelection` under `NonRandom`, and prints selected
  inputs plus change in stable order.
- [ ] T014 Prove RED with the absent executable target before implementation,
  then prove GREEN with native run, WASM build/run under `wasmtime`, and
  byte-identical output.
- [ ] T014 Run final `./gate.sh`, commit with subject
  `feat: add wasm smoke executable`, and trailer `Tasks: T014`.

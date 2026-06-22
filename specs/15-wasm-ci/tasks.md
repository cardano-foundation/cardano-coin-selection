# Tasks

## Slice 1: WASM CI Job

- [ ] T015 Add a `wasm` GitHub Actions job that builds
  `exe:coin-select-smoke` with `wasm32-wasi-cabal`.
- [ ] T015 Run the produced `coin-select-smoke.wasm` under `wasmtime` and
  compare stdout byte-for-byte with the native smoke output.
- [ ] T015 Cache `~/.ghc-wasm/.cabal/store` and upload the built `.wasm`
  artifact.
- [ ] T015 Prove the workflow with `actionlint`, local `./gate.sh`, and green
  GitHub PR checks.
- [ ] T015 Commit with subject `ci: add wasm smoke job` and trailer
  `Tasks: T015`.

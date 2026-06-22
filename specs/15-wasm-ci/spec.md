# Spec

## User Story

As a maintainer of the WASM port, I want GitHub CI to build and run the
`coin-select-smoke` executable as `wasm32-wasi`, so regressions in the WASM
target are caught before merge.

## Functional Requirements

- Add a `wasm` job to `.github/workflows/ci.yml`.
- The job must install Nix the same way the existing `build` and `docs` jobs
  do.
- The job must cache the GHC WASM cabal store at `~/.ghc-wasm/.cabal/store`.
- The job must build `exe:coin-select-smoke` with
  `wasm32-wasi-cabal --project-file=cabal-wasm.project`.
- The job must run the produced `coin-select-smoke.wasm` with `wasmtime`.
- The job must assert that WASM stdout is byte-identical to the native
  `coin-select-smoke` stdout.
- The job must upload the built `.wasm` file as an artifact.

## Success Criteria

- The PR shows a `wasm` check and it succeeds.
- Existing `build` and `docs` checks still succeed.
- Local `./gate.sh` validates the workflow syntax and the native/WASM smoke
  comparison.
- No library, executable, cabal project, or smoke source code changes are
  needed for this ticket.

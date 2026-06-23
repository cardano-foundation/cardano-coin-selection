# Tasks: release README and browser run instructions

## Slice 1: Browser run documentation

- [ ] T026-S1 Add a README "Run in the browser" entry that links the live demo
  at `https://cardano-foundation.github.io/cardano-coin-selection/demo/`.
- [ ] T026-S1 Add a docs "Run in the browser" section in
  `docs/getting-started.md`.
- [ ] T026-S1 Document the `coin-select` stdin text shape with a minimal sample.
- [ ] T026-S1 Document local `wasmtime` execution of `coin-select.wasm`.
- [ ] T026-S1 Document browser execution through
  `@bjorn3/browser_wasi_shim`, matching `web/src/bootstrap.js`.
- [ ] T026-S1 Run `./gate.sh`.
- [ ] T026-S1 Commit with subject `docs: add browser run instructions` and
  trailer `Tasks: T026-S1`.

## Release operations

- [ ] T026-R1 Resolve Q-001 and get release PR #1 checks green at `0.1.0`.
- [ ] T026-R1 Q-file parent approval before merging release PR #1.
- [ ] T026-R1 After parent approval, merge release PR #1 and verify
  `v0.1.0` tag + GitHub Release exist.
- [ ] T026-R1 Attach `coin-select.wasm` to the `v0.1.0` GitHub Release.

## Finalization

- [ ] T026-F1 Verify this task file has no open docs PR tasks before marking
  PR #36 ready for review.
- [ ] T026-F1 Drop `gate.sh` in the final ready-for-review commit.
- [ ] T026-F1 Mark PR #36 ready for review after the docs gate and PR checks
  are green.

# Tasks: release README and browser run instructions

## Slice 1: Browser run documentation

- [X] T026-S1 Add a README "Run in the browser" entry that links the live demo
  at `https://cardano-foundation.github.io/cardano-coin-selection/demo/`.
- [X] T026-S1 Add a docs "Run in the browser" section in
  `docs/getting-started.md`.
- [X] T026-S1 Add a MkDocs `Browser Demo` page at `docs/browser-demo.md` and
  nav entry that links to the deployed `/demo/` static app without generating
  a colliding `/demo/` Markdown page.
- [X] T026-S1 Cross-reference `docs/architecture/wasm-portability.md` from the
  browser demo page and explain that the same library compiled to WASM powers
  the demo.
- [X] T026-S1 Document the `coin-select` stdin text shape with a minimal sample.
- [X] T026-S1 Document local `wasmtime` execution of `coin-select.wasm`.
- [X] T026-S1 Document browser execution through
  `@bjorn3/browser_wasi_shim`, matching `web/src/bootstrap.js`.
- [X] T026-S1 Add `workflow_dispatch` to `.github/workflows/ci.yml` so release
  PR #1 checks can be started manually on `release-please--branches--main`.
- [X] T026-S1 Run `./gate.sh`.
- [X] T026-S1 Commit with subject `docs: add browser run instructions` and
  trailer `Tasks: T026-S1`.

## Release operations

- [X] T026-R1 Resolve Q-001: PR #36 carries `workflow_dispatch`, and tag-cut
  approval is granted.
- [ ] T026-R1 After PR #36 is merged and release PR #1 is green at `0.1.0`,
  merge release PR #1 and verify `v0.1.0` tag + GitHub Release exist.
- [ ] T026-R1 Attach `coin-select.wasm` to the `v0.1.0` GitHub Release.

## Finalization

- [ ] T026-F1 Verify this task file has no open docs PR tasks before marking
  PR #36 ready for review.
- [ ] T026-F1 Drop `gate.sh` in the final ready-for-review commit.
- [ ] T026-F1 Mark PR #36 ready for review after the docs gate and PR checks
  are green.

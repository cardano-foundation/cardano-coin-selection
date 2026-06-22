# Tasks: Pages deploy and CI for the SPA

## Slice 1: CI/deploy wiring and subpath smoke support

- [X] T030-S1 Add a web CI job in `.github/workflows/ci.yml` on
  `ubuntu-latest` with `paolino/dev-assets/setup-nix@v0.0.1`, ghc-wasm Cabal
  cache, and `cd web && nix develop --quiet -c just ci`.
- [X] T030-S1 Extend `.github/workflows/docs.yml` to restore/cache ghc-wasm
  Cabal directories before the deploy build.
- [X] T030-S1 Extend `.github/workflows/docs.yml` to run the web Nix/just CI,
  copy `web/dist/*` into `docs/demo/`, and then run the existing MkDocs deploy.
- [X] T030-S1 Update `web/smoke/interactive-ui.mjs` so
  `SMOKE_BASE_PATH=/demo/` serves and opens the bundle under `/demo/`.
- [X] T030-S1 Prove the focused web CI command, `/demo/` Playwright smoke, and
  actionlint command from `plan.md`.
- [X] T030-S1 Commit with subject `ci: deploy spa under docs pages` and trailer
  `Tasks: T030-S1`.

## Slice 2: Require Pages demo smoke in gate

- [X] T030-S2 Orchestrator-owned: extend `gate.sh` to run the `/demo/`
  Playwright smoke after web CI and keep actionlint on the workflow files.
- [X] T030-S2 Orchestrator-owned: prove `./gate.sh`.
- [X] T030-S2 Orchestrator-owned: commit with subject
  `chore: require pages demo smoke in gate` and trailer `Tasks: T030-S2`.

## Finalization

- [ ] T030-F1 Verify all task boxes are checked in this file.
- [ ] T030-F1 Verify PR #35 GitHub checks are green.
- [ ] T030-F1 Update the PR body with the expected live URL and validation
  evidence.
- [ ] T030-F1 Drop `gate.sh` in the final ready-for-review commit.
- [ ] T030-F1 Mark PR #35 ready for review only after checks are green.

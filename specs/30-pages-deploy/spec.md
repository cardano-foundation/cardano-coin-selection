# Specification: Pages deploy and CI for the SPA

## User Story

As a visitor to the project documentation site, I need a stable public demo URL
that serves the PureScript SPA under the existing GitHub Pages site, so I can
run coin selection in my browser without breaking the documentation pages.

## Functional Requirements

- FR-001: The existing documentation site at
  `https://cardano-foundation.github.io/cardano-coin-selection/` must remain the
  repository Pages site.
- FR-002: The SPA must be published under
  `https://cardano-foundation.github.io/cardano-coin-selection/demo/`.
- FR-003: The docs deploy workflow must build a fresh `coin-select.wasm` with
  ghc-wasm-meta before bundling the SPA.
- FR-004: The deploy workflow must build the PureScript bundle through the
  existing `web` Nix/just flow, with the freshly built wasm embedded by
  esbuild's `.wasm` binary loader.
- FR-005: The deploy workflow must copy the generated SPA bundle into
  `docs/demo/` in the CI workspace before running the existing MkDocs deploy.
- FR-006: The branch CI must gate the web app with npm install, purs-tidy,
  PureScript build, wasm-backed bundle, npm runtime budget `<= 1`, and no
  runtime install scripts.
- FR-007: The workflow runner must remain GitHub-hosted `ubuntu-latest` and use
  `paolino/dev-assets/setup-nix@v0.0.1` plus Cachix.
- FR-008: The ghc-wasm build used by the web CI/deploy path must use a GitHub
  Actions cache for `~/.ghc-wasm/.cabal/store` and
  `~/.ghc-wasm/.cabal/packages`.
- FR-009: The deployed SPA must not use CDN scripts or add runtime npm
  dependencies beyond `@bjorn3/browser_wasi_shim`.
- FR-010: The local branch gate must validate the modified workflows with
  actionlint and smoke the SPA under the `/demo/` subpath.

## Constraints

- Parent decision Q-001 approved the MkDocs `/demo/` subpath strategy. Do not
  migrate the repository Pages source to GitHub Actions artifacts.
- The public Pages deploy runs only after merge to `main`; pre-merge proof is
  workflow validation, local MkDocs/demo copy proof, and PR CI.
- `web/src/assets/coin-select.wasm`, `web/dist/index.js`, `web/node_modules/`,
  and the generated MkDocs `site/` output remain untracked.
- `gate.sh` is parent-owned and is dropped only during final PR readiness.

## Acceptance Criteria

- A valid draft PR adds CI/deploy wiring that preserves docs at `/` and serves
  the SPA at `/demo/` after merge.
- The deploy workflow builds wasm fresh, builds `web/dist`, copies it into
  `docs/demo/`, then runs the existing MkDocs deploy path.
- The PR CI web job enforces purs-tidy, build, bundle, npm runtime budget, and
  no runtime install scripts.
- `./gate.sh` passes locally and includes actionlint plus a browser smoke for
  the `/demo/` subpath.
- GitHub PR checks pass before the ticket is marked complete.

# Specification: PureScript SPA scaffold and wasm loader

## User Story

As a visitor to the browser demo planned by parent epic #25, I need a
client-side page that loads the in-repo `coin-select` wasm executable and runs
the default text-line selection example, so I can see the Haskell coin
selection code execute in the browser without a backend.

## Functional Requirements

- FR-001: The repository must contain a `web/` PureScript SPA scaffold using
  Halogen, Spago 2, esbuild, and a local Nix flake based on
  `purescript-overlay` plus `mkSpagoDerivation`.
- FR-002: `web/spago.yaml` must pin registry `72.1.0` and use Halogen without
  React, MUI, Emotion, or other npm UI dependencies.
- FR-003: `web/package.json` must pin exact npm versions and include only
  `@bjorn3/browser_wasi_shim` as a runtime dependency.
- FR-004: `web/.npmrc` must set `ignore-scripts=true`.
- FR-005: `web/package-lock.json` and `web/spago.lock` must be committed.
- FR-006: The web build must embed `web/src/assets/coin-select.wasm` via
  esbuild `--loader:.wasm=binary`; the binary is produced locally from the
  in-repo `coin-select` executable and must not be committed.
- FR-007: `web/src/bootstrap.js` must expose
  `globalThis.runCoinSelect(stdinText) -> Promise<string>` using
  `@bjorn3/browser_wasi_shim` to feed stdin and capture stdout.
- FR-008: PureScript FFI must follow the globalThis-seeded convention: FFI
  `.js` files read `globalThis.runCoinSelect` and do not import npm modules.
- FR-009: `web/src/Main.purs` must render a minimal Halogen app that runs the
  default input on startup and displays the returned `selected ...` and
  `change ...` output lines.
- FR-010: The web gate must run `purs-tidy` checks, `spago build`, bundling,
  and npm budget checks from `gate.sh`.

## Constraints

- The page is fully client-side. No backend calls, runtime CDN scripts, or
  remote wasm fetches are allowed.
- Runtime npm package budget is <= 1 package.
- Plain CSS only; no MUI, Emotion, or component-library npm tree.
- The default stdin example is the S1 text contract:

  ```text
  utxo input-1 2000000
  utxo input-2 3000000
  output target-address 4500000
  ```

## Acceptance Criteria

- `./gate.sh` builds `coin-select.wasm` into `web/src/assets/` and runs the web
  `just ci` gate successfully.
- `cd web && nix build --quiet` or
  `cd web && nix develop --quiet -c just bundle` produces a static bundle
  containing `index.html` and `index.js`.
- The built page runs the default selection in the browser and renders output
  containing `selected input-1 2000000`, `selected input-2 3000000`, and
  `change 500000`.
- PR #33 stays draft until GitHub CI is green.

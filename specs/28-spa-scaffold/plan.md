# Plan: PureScript SPA scaffold and wasm loader

## Technical Approach

Add a self-contained `web/` flake so the SPA can be built independently from
the Haskell package while still consuming the wasm executable produced from
this repository. The root `gate.sh` owns the cross-toolchain step: build
`exe:coin-select` with `wasm32-wasi-cabal`, copy the resulting
`coin-select.wasm` into `web/src/assets/`, then run the web Nix dev-shell gate.

The web app follows the project-family PureScript stack exactly: Halogen UI,
Spago 2 with registry `72.1.0`, esbuild, `purescript-overlay`,
`mkSpagoDerivation`, and `@bjorn3/browser_wasi_shim` as the only runtime npm
dependency. `bootstrap.js` imports the WASI shim and the wasm bytes, exposes
`globalThis.runCoinSelect`, and PureScript calls that function through a thin
FFI module.

The shared PureScript skill template still names `nodejs_20`, but the locked
current nixpkgs refuses Node 20 as EOL/insecure. Per parent decision
Q-001-nodejs20-insecure, this scaffold uses maintained `nodejs_22` for the
build shell and bundler while keeping the runtime npm budget unchanged.
Node itself is not shipped in the static SPA artifact.

Use the default S1 text contract as the first rendered demo. The app should run
that input on startup and render the returned stdout lines. S3 will add editable
inputs; this ticket should stay a minimal scaffold plus working loader.

## Files

- `gate.sh` is already bootstrapped and is the final branch gate.
- `web/flake.nix` and `web/flake.lock` define the PureScript build and dev
  shell.
- `web/spago.yaml` and `web/spago.lock` pin PureScript dependencies.
- `web/package.json`, `web/package-lock.json`, and `web/.npmrc` pin and harden
  npm dependencies.
- `web/justfile` exposes `install`, `lint`, `build`, `bundle`, `ci`, and a wasm
  preparation recipe if useful.
- `web/dist/index.html` is the static shell with plain CSS and no CDN scripts.
- `web/src/bootstrap.js` embeds the wasm and seeds `globalThis.runCoinSelect`.
- `web/src/Main.purs` contains the Halogen entry point.
- `web/src/FFI/CoinSelect.purs` and `web/src/FFI/CoinSelect.js` provide the
  PureScript bridge to the global runner.
- `.gitignore` may be extended to ignore generated web assets such as
  `web/src/assets/coin-select.wasm`, `web/output/`, and Nix result links.

## Slice Breakdown

### Slice 1: SPA scaffold, wasm loader, and local gate

One bisect-safe implementation commit creates the web scaffold, pins lockfiles,
loads the in-repo wasm through the WASI shim, renders the default selection
output, and proves the npm budget plus web build. This is one slice because the
toolchain files, wasm asset path, bootstrap loader, PureScript FFI, and Halogen
entry point must agree for HEAD to build.

## Verification

- RED is documented as skipped for frontend unit tests because this repo has no
  PureScript test harness yet; proof is build and browser smoke.
- Focused proof: `cd web && nix develop --quiet -c just ci`.
- Bundle proof: `cd web && nix build --quiet` or
  `cd web && nix develop --quiet -c just bundle`, depending on whether the
  generated wasm asset can be modeled in the local flake without committing the
  binary.
- Full branch proof: `./gate.sh`.
- Browser smoke: serve `web/result` or `web/dist` locally and verify the page
  renders `selected input-1 2000000`, `selected input-2 3000000`, and
  `change 500000`.
- Budget proof:
  `jq '[.packages | to_entries[] | select(.key != "" and (.value.dev != true))] | length' web/package-lock.json`
  must be <= 1, and the install-script count must be 0.

## Risks

- `@bjorn3/browser_wasi_shim` API shape must match the skill pattern. Use exact
  version `0.4.2`.
- The build shell uses `nodejs_22` because current nixpkgs marks `nodejs_20`
  insecure/EOL. This is a documented exception to the stale skill template, not
  a runtime dependency increase.
- The gate writes a generated wasm file under `web/src/assets/`; it must remain
  untracked while still being present before bundling.
- `spago bundle --offline` in Nix requires a committed `spago.lock`.
- S4 owns GitHub Pages deployment and final CI build/deploy wiring, so this PR
  should not add Pages workflows.

# Plan

## Scope

Gate the existing deterministic WASM smoke executable in GitHub Actions. The
previous child already added `cabal-wasm.project` and `coin-select-smoke`;
this ticket wires that proof into CI.

## Verified Facts

- `.github/workflows/ci.yml` currently has `build` and `docs` jobs.
- Both existing jobs use `paolino/dev-assets/setup-nix@v0.0.1` with
  `CACHIX_AUTH_TOKEN`.
- `cabal-wasm.project` builds `exe:coin-select-smoke` with the
  `ghc-wasm-meta` `all_9_12` toolchain.
- The prior local gate proves the native and WASM smoke outputs can be compared
  byte-for-byte.

## Slice 1: WASM CI Job

Owned files:

- `.github/workflows/ci.yml`

Forbidden scope:

- Do not edit `lib/`.
- Do not edit `app/`.
- Do not edit `test/`.
- Do not edit `cardano-coin-selection.cabal`.
- Do not edit `cabal-wasm.project`.
- Do not edit flake, Nix, or dependency pin files.
- Do not change fixed-output hashes.

Work:

- Add a `wasm` job that mirrors the existing workflow style and Nix setup.
- Restore or cache the GHC WASM cabal store at `~/.ghc-wasm/.cabal/store`.
- Build native `exe:coin-select-smoke` and capture stdout.
- Build WASM `exe:coin-select-smoke` with:
  `nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12'
  --command wasm32-wasi-cabal --project-file=cabal-wasm.project build
  exe:coin-select-smoke`.
- Locate the generated `coin-select-smoke.wasm`, run it with `wasmtime`, and
  compare stdout against the native output with `diff -u`.
- Upload the generated `.wasm` through `actions/upload-artifact`.

Proof:

- RED: record that the current workflow lacks a `wasm` job.
- GREEN:
  - `nix shell 'nixpkgs#actionlint' --command actionlint .github/workflows/ci.yml`
  - `./gate.sh`
  - after push, all GitHub PR checks including `wasm` are green.

Commit:

- `ci: add wasm smoke job`
- body trailer: `Tasks: T015`

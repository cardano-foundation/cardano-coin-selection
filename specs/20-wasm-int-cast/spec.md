# Issue #20: WASM `int-cast` Fix

## User Story

As a maintainer preparing `cardano-coin-selection` for `wasm32-wasi`, I want
the existing `int-cast` dependency to build under `ghc-wasm-meta` so the
library WASM build links successfully without broad native-code changes.

## Requirements

- Preserve the existing native `int-cast` dependency and library API.
- Prefer the cheapest working fix:
  1. a `cabal-wasm.project` package stanza for `int-cast`;
  2. a patched `paolino/int-cast` fork with nix32 `--sha256`;
  3. a vendored safe-cast replacement only if the first two are worse.
- Use the verified project-file workaround:
  `package int-cast` with `ghc-options: -DHTYPE_SIG_ATOMIC_T=HTYPE_INT`.
- Do not create or push an `int-cast` fork unless a parent Q/A approval is
  recorded first.
- Keep the existing `paolino/ram` pin and WASI mmap options unchanged.
- Keep native CI green.
- Make `./gate.sh` prove the full wasm library build now succeeds.

## Acceptance

- `wasm32-wasi-cabal --project-file=cabal-wasm.project build
  lib:cardano-coin-selection` succeeds.
- `nix develop --quiet -c just CI` succeeds.
- `./gate.sh` succeeds end to end.
- No new `source-repository-package` is added for `int-cast`.
- If a source-repository-package is added despite the plan, it includes a
  nix32 `--sha256` pin and a parent-approved Q-file.

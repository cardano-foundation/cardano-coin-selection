# Issue #12: WASM ByteArray Dependency Selection

## User Story

As a maintainer preparing `cardano-coin-selection` for `wasm32-wasi`, I want
the library to avoid the WASM-hostile `memory` package on wasm targets while
preserving the existing native dependency and source surface.

## Requirements

- In the library stanza of `cardano-coin-selection.cabal`, select `ram` for
  `arch(wasm32)` builds and keep `memory` for all other architectures.
- Use bounds that resolve at the repository index-state
  `2025-10-01T00:00:00Z`: `ram >=0.22 && <0.23` and
  `memory >=0.15 && <0.20`.
- Confirm that `ram` provides the `Data.ByteArray.ByteArrayAccess` surface used
  by `lib/Cardano/CoinSelection/Types/Hash.hs`.
- Do not change `Hash.hs` unless the surface check proves it is necessary; if a
  source change is necessary, keep it minimal and document the reason in the PR.

## Acceptance

- Native `./gate.sh` passes, covering the current native CI recipe.
- A real verification confirms the `arch(wasm32)` branch selects `ram` or that
  `ram` exports `Data.ByteArray.ByteArrayAccess`.
- `lib/Cardano/CoinSelection/Types/Hash.hs` is unchanged, or any change is
  minimal and justified in the PR.
- The PR states that a full wasm build is intentionally deferred to #13 and
  #14.

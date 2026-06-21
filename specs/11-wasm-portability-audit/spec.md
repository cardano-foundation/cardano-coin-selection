# Issue #11: WASM Dependency Portability Audit

## User Story

As a maintainer preparing `cardano-coin-selection` for `wasm32-wasi`, I want a
clear dependency portability audit so the follow-up implementation tickets know
which packages are already portable, which package blocks WASI, and what
mitigation is planned.

## Requirements

- Document every direct dependency of the library stanza in
  `cardano-coin-selection.cabal`.
- Record whether `lib/` uses any `foreign import`.
- Identify `memory` as the only known WASM-hostile dependency and show that it is
  used through `ByteArrayAccess` in
  `lib/Cardano/CoinSelection/Types/Hash.hs`.
- Record the planned mitigation for `memory`: switch to `ram` under
  `arch(wasm32)` in issue #12.
- Record that `hspec`, `QuickCheck`, and `pretty-simple` are public library
  dependencies through generator/law modules, and that issue #16 tracks moving
  test helpers out of the public library surface.
- Add the new document to the MkDocs navigation.

## Acceptance

- `docs/architecture/wasm-portability.md` exists and contains a direct
  dependency matrix with status and mitigation columns.
- `mkdocs.yml` links the page from the navigation.
- The change is documentation-only apart from the PR-local `gate.sh` and
  orchestration specs.
- `./gate.sh` passes.

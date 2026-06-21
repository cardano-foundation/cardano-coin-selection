# Plan

## Scope

Produce the A1 documentation deliverable for issue #11. Do not change Haskell
source, cabal dependencies, Nix files, CI, or generated artifacts.

## Verified Facts

- `rg "foreign import" lib` returns no matches.
- `memory` is the only dependency called out as WASM-hostile for this audit.
- `memory` is used in `lib/Cardano/CoinSelection/Types/Hash.hs` via
  `Data.ByteArray.ByteArrayAccess`, derived for `Hash`.
- The public library dependency list includes test-support packages
  (`hspec`, `QuickCheck`, `pretty-simple`) because `.Gen` modules and
  `Cardano.CoinSelection.Test.Laws` are exposed; issue #16 owns that cleanup.

## Slice 1: Documentation Audit

Owned files:

- `docs/architecture/wasm-portability.md`
- `mkdocs.yml`

Work:

- Create the architecture directory if needed.
- Add a concise audit page with:
  - summary/result;
  - local audit commands/evidence;
  - direct dependency matrix;
  - `memory` blocker details and #12 mitigation;
  - public test-helper dependency note and #16 mitigation;
  - out-of-scope statement that this PR makes no cabal/code changes.
- Add the page to the MkDocs navigation under a new `Architecture` section.

Proof:

- `./gate.sh`

Commit:

- `docs: add WASM portability audit`
- body trailer: `Tasks: T011`

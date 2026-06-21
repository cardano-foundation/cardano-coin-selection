# Plan

## Scope

Implement the A2 dependency-selection deliverable for issue #12. This ticket
changes only the cabal dependency expression unless verification proves a
minimal source change is required.

## Verified Facts

- Issue #11's audit identifies `memory` as the only known WASM-hostile direct
  library dependency.
- The only repository-local `Data.ByteArray` use is
  `lib/Cardano/CoinSelection/Types/Hash.hs`, which derives
  `ByteArrayAccess` for `Hash`.
- The flake sets the package index-state to `2025-10-01T00:00:00Z`.
- The repo's native gate is `./gate.sh`, which runs `git diff --check` and
  `nix develop --quiet -c just CI`.

## Slice 1: Conditional ByteArray Dependency

Owned files:

- `cardano-coin-selection.cabal`

Work:

- Replace the unconditional library `memory` dependency with:

  ```cabal
  if arch(wasm32)
    build-depends:
      , ram                     >=0.22    && <0.23
  else
    build-depends:
      , memory                  >=0.15    && <0.20
  ```

- Preserve the existing native `memory` bounds and style.
- Confirm that no source change is needed for `Data.ByteArray.ByteArrayAccess`,
  preferably with a dry-run dependency solve or a direct module-export check
  against the selected `ram` version.

Proof:

- `./gate.sh`
- ByteArrayAccess surface confirmation recorded in the worker STATUS and PR
  body.

Commit:

- `build: use ram for wasm32 bytearray support`
- body trailer: `Tasks: T012`

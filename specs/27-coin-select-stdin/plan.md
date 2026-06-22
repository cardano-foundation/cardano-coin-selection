# Plan: text stdin coin selection executable

## Technical Approach

The existing `coin-select-smoke` executable already shows the deterministic
selection context, constraints, and rendering order. The new `coin-select`
should reuse that construction pattern while replacing hardcoded UTxOs and
outputs with a small line-oriented text stdin contract.

Use only lightweight executable dependencies. The contract is space-separated
text:

```text
utxo input-1 2000000
utxo input-2 3000000
utxo input-3 5000000
output target-address 4500000
```

Expected output for that sample:

```text
selected input-1 2000000
selected input-2 3000000
change 500000
```

Output must be deterministic: selected inputs are sorted by UTxO id, and change
bundles are sorted by lovelace before rendering.

The implementation must also update the wasm CI job so GitHub checks prove the
new executable, not only the pre-existing hardcoded smoke executable.

## Files

- `app/Main.hs` may be refactored only to share the smoke selection context or
  helper functions.
- `app/CoinSelect.hs` should contain the new executable entry point.
- Additional `app/*.hs` helper modules are allowed if they reduce duplication
  between `coin-select-smoke` and `coin-select`.
- `cardano-coin-selection.cabal` will define `exe:coin-select` without adding
  JSON dependencies.
- `justfile` will add a lowercase `ci` alias without removing `CI`.
- `.github/workflows/ci.yml` will extend the wasm job to build/run
  `coin-select` and compare native/wasm output.
- `gate.sh` already contains the ticket-specific local proof and should remain
  green at the end.

## Slice Breakdown

### Slice 1: stdin text executable and wasm proof

One bisect-safe implementation commit adds the CLI executable, wires native and
wasm build metadata, extends CI, and proves native/wasm byte identity with the
sample text. This is intentionally one slice because the cabal target, parser,
native smoke, wasm smoke, gate, and CI workflow are one acceptance unit; a
partial commit would leave HEAD unable to satisfy the ticket.

## Verification

- Focused RED: `nix develop --quiet -c cabal build -O0 exe:coin-select`
  should fail before the new executable is wired.
- Native proof: `printf '%s\n' '<sample-text>' | nix develop --quiet -c cabal run -O0 exe:coin-select`.
- CI proof: `nix develop --quiet -c just ci`.
- WASM proof:
  `nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12' --command bash -c 'wasm32-wasi-cabal --project-file=cabal-wasm.project build exe:coin-select && wasmtime <built-wasm>'`.
- Final proof: `./gate.sh`.

## Risks

- A previous JSON/aeson attempt crossed the focused wasm proof but made the full
  gate's wasm stage too heavy for the lean surface goal. Keep this slice on the
  text format unless the parent explicitly changes the contract again.
- Cabal component layout may require `other-modules` entries if code is shared
  between `coin-select-smoke` and `coin-select`.
- GitHub CI currently proves only `coin-select-smoke` for wasm, so it must be
  extended or the remote `wasm` check will not cover this issue.

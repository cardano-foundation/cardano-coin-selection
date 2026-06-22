# Issue #14: WASM Smoke Executable

## User Story

As a maintainer preparing `cardano-coin-selection` for `wasm32-wasi`, I want
a small executable that runs a representative coin selection natively and
under `wasmtime`, so we prove the linked WASM library can execute real
selection logic and produce deterministic output.

## Requirements

- Add an executable component named `coin-select-smoke`.
- Keep the executable source under `app/` and keep dependencies minimal:
  `base`, `cardano-coin-selection`, and only extra core packages if required
  for stable printing.
- Define a small local `SelectionContext` with simple ordered/showable address
  and UTxO identifiers.
- Construct a valid `SelectionConstraints` value using deterministic,
  permissive constraints:
  zero minimum ada, zero fee, unlimited token-bundle size, no collateral, and
  large output quantity limits.
- Construct a valid `SelectionParams` value with a 2-3 entry UTxO set, at
  least one target output, no minting or burning, no extra coin in/out, no
  collateral requirement, and a deterministic selection strategy.
- Run `Cardano.CoinSelection.performSelection` in
  `Control.Monad.Random.NonRandom`, not in `IO` randomness.
- Print a stable textual summary of the result to stdout:
  selected inputs and generated change must be sorted or otherwise emitted in
  a deterministic order.
- Build and run the executable natively.
- Build the same executable with
  `wasm32-wasi-cabal --project-file=cabal-wasm.project build
  exe:coin-select-smoke`.
- Run the generated `coin-select-smoke.wasm` with `wasmtime` and compare its
  stdout byte-for-byte with the native run.
- Keep native `nix develop --quiet -c just CI` green.
- Keep `./gate.sh` as the final mechanical gate for native CI, native smoke,
  WASM build, wasmtime run, and native/WASM output comparison.

## Acceptance

- `nix develop --quiet -c cabal run -O0 exe:coin-select-smoke` prints the
  expected deterministic selection summary.
- `nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12'
  --command wasm32-wasi-cabal --project-file=cabal-wasm.project build
  exe:coin-select-smoke` succeeds.
- Running the produced `coin-select-smoke.wasm` with `wasmtime` prints exactly
  the same bytes as the native run.
- `./gate.sh` exits 0 at HEAD and covers the native and WASM smoke comparison.
- GitHub CI is green before the ticket reports `COMPLETE`.

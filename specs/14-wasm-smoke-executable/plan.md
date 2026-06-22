# Plan

## Scope

Implement the A4 deliverable for issue #14: a deterministic native and WASM
smoke executable that proves the already-linking library can execute
`performSelection` under `wasmtime`.

## Verified Facts

- `main` already contains `cabal-wasm.project` with the patched
  `paolino/ram` source pin and the `int-cast` `HTYPE_SIG_ATOMIC_T` stanza.
- The library exports `Cardano.CoinSelection.performSelection`,
  `SelectionConstraints`, `SelectionParams`, `Selection`, and primitive
  `Coin`/`TokenBundle` types.
- The library exports `Control.Monad.Random.NonRandom`, whose
  `runNonRandom` wrapper makes `MonadRandom` calls deterministic.
- `SelectionContext` only requires ordered and showable associated `Address`
  and `UTxO` types.
- `UTxOIndex.fromMap` and `UTxOSelection.fromIndex` can build the available
  ordinary-input set from a small `Map`.
- The PR-local `gate.sh` already runs native CI and activates the smoke
  native/WASM comparison as soon as the executable component exists.

## Slice 1: Deterministic Smoke Executable

Owned files:

- `app/Main.hs`
- `cardano-coin-selection.cabal`
- `cabal-wasm.project` only if the executable needs an explicit WASM project
  stanza beyond the existing package entry.
- `gate.sh` only for mechanical fixes that make the already-required native
  run, WASM build/run, and byte comparison execute correctly. Do not weaken or
  skip the smoke checks once the executable exists.

Forbidden scope:

- Do not edit `lib/`.
- Do not edit `test/`.
- Do not add test-only generator imports to the executable.
- Do not add dependencies beyond `base`, `cardano-coin-selection`, and a
  narrowly justified core package if the implementation truly needs it.
- Do not change the patched `paolino/ram` pin or any fixed-output hash.

Work:

- Add `executable coin-select-smoke` to `cardano-coin-selection.cabal` with
  `hs-source-dirs: app`, `main-is: Main.hs`, `import: language, opts-exe`, and
  minimal build dependencies.
- Make the executable buildable for native and `arch(wasm32)`. If the existing
  `cabal-wasm.project` can build `exe:coin-select-smoke` without changes,
  leave it unchanged.
- In `app/Main.hs`, define a small smoke context with ordered/showable address
  and UTxO IDs.
- Build a 2-3 entry UTxO index of ada-only `TokenBundle.fromCoin` values and
  one target output.
- Use permissive constraints: zero minimum ada, zero fee, unlimited token
  bundle size, no collateral requirement, high output quantity limits, and
  stable dummy addresses.
- Run `runNonRandom $ runExceptT $ performSelection constraints params`.
- On success, print selected inputs and change in a stable sorted textual
  format. On failure, print the selection error and exit non-zero.
- Ensure native and WASM output are byte-identical.

Proof:

- RED: before adding the executable, record that
  `nix develop --quiet -c cabal run -O0 exe:coin-select-smoke` fails because
  the target does not exist. No RED source diff is expected for this
  executable slice; the failing target command is the RED evidence.
- GREEN:
  - `nix develop --quiet -c cabal run -O0 exe:coin-select-smoke`
  - `nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12'
    --command bash -c "wasm32-wasi-cabal --project-file=cabal-wasm.project
    build exe:coin-select-smoke && wasmtime $(find dist-newstyle -name
    'coin-select-smoke.wasm' -type f | head -1)"`
  - `./gate.sh`

Commit:

- `feat: add wasm smoke executable`
- body trailer: `Tasks: T014`

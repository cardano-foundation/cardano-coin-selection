# References

## Blog posts

- **Self Organisation in Coin Selection**
  ([IOHK blog](https://iohk.io/blog/self-organisation-in-coin-selection/))
  -- describes how random coin selection leads to a self-organising UTxO
  distribution that mirrors the user's payment patterns.

- **The Challenges of Optimizing Unspent Output Selection**
  ([Jameson Lopp](https://medium.com/@lopp/the-challenges-of-optimizing-unspent-output-selection-a3e5d05d13ef))
  -- a survey of coin selection approaches for Bitcoin, many of which apply
  to Cardano's UTxO model.

## Cardano documentation

- **Cardano Wallet User Guide**
  ([GitHub Pages](https://cardano-foundation.github.io/cardano-wallet/))
  -- the full documentation for the cardano-wallet project, which uses
  this coin selection library.

- **Cardano Ledger Specification**
  ([GitHub](https://github.com/intersectmbo/cardano-ledger))
  -- the formal specification of the Cardano ledger, including transaction
  validation rules that constrain coin selection.

## Primitives

This library ships its own primitive types and numeric utilities -- there are
no external dependencies on `cardano-numeric` or `cardano-wallet-primitive`.
Types such as `TokenMap`, `TokenBundle`, and `Coin`, as well as functions like
`partitionNatural` and `equipartitionNatural`, are all defined within this
package.

## Haddock documentation

The most detailed documentation is in the Haddock comments within the source
code itself:

| Module | Key documentation |
|--------|-------------------|
| `Cardano.CoinSelection.Balance` | Round-robin algorithm, change generation, minting/burning |
| `Cardano.CoinSelection.Collateral` | Dual-strategy collateral selection |
| `Cardano.CoinSelection.UTxOIndex.Internal` | Index invariants, selection filters |
| `Cardano.CoinSelection.UTxOSelection` | State machine semantics |
| `Cardano.CoinSelection.Internal.Numeric` | padCoalesce, partitionNatural proofs |
| `Cardano.CoinSelection.Types.TokenMap` | Partial ordering rationale |

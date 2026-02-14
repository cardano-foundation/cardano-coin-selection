# cardano-coin-selection

[![CI](https://github.com/cardano-foundation/cardano-coin-selection/actions/workflows/ci.yml/badge.svg)](https://github.com/cardano-foundation/cardano-coin-selection/actions/workflows/ci.yml)
[![Docs](https://github.com/cardano-foundation/cardano-coin-selection/actions/workflows/docs.yml/badge.svg)](https://cardano-foundation.github.io/cardano-coin-selection/)

Self-contained coin selection library extracted from
[cardano-wallet](https://github.com/cardano-foundation/cardano-wallet).

Provides balance-aware coin selection, collateral selection, and UTxO
index/selection data structures. Ships its own lightweight primitive
types (`Coin`, `TokenBundle`, `TokenMap`, etc.) so it has zero Cardano
ecosystem dependencies — only Hackage packages.

## Documentation

Full documentation is available at
**<https://cardano-foundation.github.io/cardano-coin-selection/>**.

## License

[Apache-2.0](LICENSE) — Copyright 2018-2022 IOHK, 2023-2026 Cardano Foundation.

# Getting Started

## Installation

Add `cardano-coin-selection` to your `build-depends` in your `.cabal` file:

```cabal
build-depends:
    , cardano-coin-selection
```

The package is available from the [Cardano Haskell Package repository (CHaP)](https://github.com/intersectmbo/cardano-haskell-packages).

Add CHaP to your `cabal.project`:

```
repository cardano-haskell-packages
  url: https://chap.intersectmbo.org/
  secure: True
```

## Run in the browser

The hosted browser demo is available at
<https://cardano-foundation.github.io/cardano-coin-selection/demo/>.

The demo runs the stdin-driven `coin-select.wasm` executable. The input is a
line-oriented text document: use `utxo <id> <lovelace>` for available UTxOs and
`output <address> <lovelace>` for target outputs. Blank lines are ignored.

```text
utxo input-1 2000000
utxo input-2 3000000
utxo input-3 5000000
output target-address 4500000
```

To run the same executable locally, save the text as `input.txt` and pass it to
`wasmtime` on stdin:

```sh
wasmtime coin-select.wasm < input.txt
```

In the browser bundle, `web/src/bootstrap.js` imports `coin-select.wasm` and
uses `@bjorn3/browser_wasi_shim` to provide WASI stdin, stdout, and stderr
around the same executable.

## Module imports

The library is designed around qualified imports:

```haskell
import qualified Cardano.CoinSelection as CS
import qualified Cardano.CoinSelection.Balance as Balance
import qualified Cardano.CoinSelection.Collateral as Collateral
import qualified Cardano.CoinSelection.UTxOIndex as UTxOIndex
import qualified Cardano.CoinSelection.UTxOSelection as UTxOSelection
```

## Defining a selection context

Before performing a selection, you need to define a `SelectionContext` that
specifies your address and UTxO identifier types:

```haskell
data MyContext

instance SelectionContext MyContext where
    type Address MyContext = MyAddress
    type UTxO    MyContext = MyTxIn
```

The only requirements are that both associated types have `Ord` and `Show`
instances.

## Performing a selection

The main entry point is `Cardano.CoinSelection.performSelection`:

```haskell
import Cardano.CoinSelection
    ( Selection
    , SelectionConstraints (..)
    , SelectionError
    , SelectionParams (..)
    , performSelection
    )

result <- runExceptT $ performSelection constraints params
-- result :: Either (SelectionError MyContext) (Selection MyContext)
```

The `performSelection` function:

1. Selects inputs from the UTxO set to cover user-specified outputs
2. Selects inputs to cover collateral (if required)
3. Produces change outputs to return excess value
4. Balances the selection to pay for the transaction fee

See the [Tutorial](tutorial.md) for a complete worked example.

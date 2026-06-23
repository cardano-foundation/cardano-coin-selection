# Browser Demo

Open the hosted browser demo:

<https://cardano-foundation.github.io/cardano-coin-selection/demo/>

The demo is powered by the same `cardano-coin-selection` library as the Haskell
package. The command-line executable is compiled to `coin-select.wasm`, then the
browser bundle runs that WASM module with WASI stdin, stdout, and stderr.

The input format matches the local WASM example from
[Getting Started](getting-started.md#run-in-the-browser): provide available
UTxOs with `utxo <id> <lovelace>` lines and target outputs with
`output <address> <lovelace>` lines.

The portability notes for this build live in the
[WASM Portability Audit](architecture/wasm-portability.md).

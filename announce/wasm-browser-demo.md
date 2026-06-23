# Browser WASM demo announcement draft

The `cardano-coin-selection` project now includes a browser-runnable WASM demo
for the Cardano wallet coin-selection algorithm.

The demo uses the Haskell implementation extracted from `cardano-wallet`,
compiled to `wasm32-wasi`, and runs fully client-side in the browser. The goal
is to use one audited implementation across native and browser contexts, without
maintaining a JavaScript reimplementation of the selection algorithm.

Live demo:

<https://cardano-foundation.github.io/cardano-coin-selection/demo/>

Release:

<https://github.com/cardano-foundation/cardano-coin-selection/releases/tag/v0.1.0>

Related notes:

- Browser demo docs: <https://cardano-foundation.github.io/cardano-coin-selection/browser-demo/>
- WASM portability notes: <https://cardano-foundation.github.io/cardano-coin-selection/architecture/wasm-portability/>

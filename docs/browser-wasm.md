# Browser WASM

The browser build runs the Cardano wallet coin-selection algorithm in a web
page. It uses the Haskell implementation extracted from `cardano-wallet`,
compiled to `wasm32-wasi`, and runs fully client-side in the browser.

This keeps one audited Haskell implementation for native and browser use,
without a JavaScript reimplementation of the selection algorithm.

Try the live demo:

<https://cardano-foundation.github.io/cardano-coin-selection/demo/>

The browser WASM asset is part of the v0.1.0 release:

<https://github.com/cardano-foundation/cardano-coin-selection/releases/tag/v0.1.0>

The [Browser Demo](browser-demo.md) page describes the demo input format. The
[WASM Portability Audit](architecture/wasm-portability.md) records the
portability work behind the build.

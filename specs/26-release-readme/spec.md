# Spec: Release README and browser run instructions

## Priority User Story

As a release consumer, I can find the hosted browser demo and understand how
the `coin-select.wasm` executable is run locally and in the browser before the
first public release is cut.

## Acceptance Criteria

- README links the public documentation and the live browser demo at
  `https://cardano-foundation.github.io/cardano-coin-selection/demo/`.
- Project documentation includes a "Run in the browser" section.
- The docs explain the stdin-driven `coin-select.wasm` asset used by the SPA.
- The docs include a local `wasmtime` invocation shape for the `coin-select`
  executable.
- The docs mention that the browser bundle uses `@bjorn3/browser_wasi_shim`
  to provide WASI stdin/stdout around `coin-select.wasm`.
- Strict MkDocs build passes.

## Release Acceptance

- Release PR #1 is green at version `0.1.0` before requesting parent approval
  to merge/tag.
- After approval, merging release PR #1 creates tag/release `v0.1.0`.
- The GitHub Release for `v0.1.0` has `coin-select.wasm` attached.

## Out of Scope

- Hackage publication.
- Haskell source, cabal, Nix, web application, or CI changes unless parent
  answers Q-001 by explicitly widening scope for the CI dispatch unblock.

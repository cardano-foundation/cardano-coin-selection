# Tasks: PureScript SPA scaffold and wasm loader

## Slice 1: SPA scaffold, wasm loader, and local gate

- [ ] T028-S1 Create the `web/` PureScript Halogen scaffold with Nix, Spago, esbuild, lockfiles, `.npmrc`, and exact `@bjorn3/browser_wasi_shim` dependency.
- [ ] T028-S1 Add the wasm asset preparation path and keep generated `coin-select.wasm` out of git.
- [ ] T028-S1 Implement `bootstrap.js` so `globalThis.runCoinSelect(stdinText)` runs the embedded WASI wasm and returns captured stdout.
- [ ] T028-S1 Implement the PureScript FFI bridge and Halogen `Main` that runs the default S1 input and renders the returned output lines.
- [ ] T028-S1 Prove `cd web && nix develop --quiet -c just ci`, `./gate.sh`, npm budget checks, bundle output, and browser smoke.
- [ ] T028-S1 Commit with subject `feat: scaffold purescript spa wasm loader` and trailer `Tasks: T028-S1`.

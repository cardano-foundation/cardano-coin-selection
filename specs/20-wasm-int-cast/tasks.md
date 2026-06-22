# Tasks

## Slice 1: Project-file `int-cast` WASM workaround

- [X] T020 Add a `package int-cast` stanza to `cabal-wasm.project` with
  `ghc-options: -DHTYPE_SIG_ATOMIC_T=HTYPE_INT`.
- [X] T020 Verify the pre-change WASM failure and the post-change full
  `./gate.sh` success.
- [X] T020 Commit with subject
  `build: teach wasm project int-cast sig_atomic_t` and trailer
  `Tasks: T020`.

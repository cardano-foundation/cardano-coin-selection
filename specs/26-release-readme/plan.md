# Plan: Release README and browser run instructions

## Context

Issue #26 has two independent tracks:

1. A normal docs PR on `feat/release-readme`, gated by `./gate.sh`.
2. Release-please operations on PR #1, ending in `v0.1.0` and a
   `coin-select.wasm` GitHub Release asset.

Q-001 approved widening PR #36 to add `workflow_dispatch` to `CI`, because the
release branch currently lacks the manual trigger that the ticket brief
expected.

## Slice Breakdown

### Slice 1: Browser run documentation

Driver/navigator owned files:

- `README.md`
- `docs/getting-started.md`
- `.github/workflows/ci.yml`

The slice adds concise user-facing instructions for:

- the live browser demo URL,
- the stdin text shape accepted by `coin-select`,
- local `wasmtime coin-select.wasm < input.txt`,
- browser integration through `@bjorn3/browser_wasi_shim`, matching
  `web/src/bootstrap.js`.
- `workflow_dispatch` under the existing `CI` workflow `on:` block.

Proof:

- `./gate.sh`

Commit:

- `docs: add browser run instructions`
- `Tasks: T026-S1`

## Release Operations

The ticket-orchestrator owns release operations:

- reconcile release PR #1 down to `0.1.0`,
- get required checks green,
- Q-file the parent before merge/tag,
- after approval, merge release PR #1 and upload `coin-select.wasm`.

Tag-cut approval has been granted by the epic owner relay. After PR #36 merges,
the orchestrator proceeds through release PR #1, tag creation, and release asset
upload without a second tag-approval Q-file.

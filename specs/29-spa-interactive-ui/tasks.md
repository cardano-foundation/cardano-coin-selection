# Tasks: SPA interactive coin-selection UI

## Slice 1: Interactive UI and browser smoke

- [X] T029-S1 Replace the fixed S2 output view with an editable Halogen input area seeded with the default text contract.
- [X] T029-S1 Validate local text input before calling the wasm and display clear on-page errors for empty or malformed input.
- [X] T029-S1 Run the existing `runCoinSelect` FFI on initial load and on submit, parse stdout into selected rows plus change, and render the structured result.
- [X] T029-S1 Update the plain CSS in `web/dist/index.html` for the textarea, action row, status/error display, result table, and responsive layout.
- [X] T029-S1 Add `web/smoke/interactive-ui.mjs` using Nix-provided Playwright to edit the input and assert the recomputed selected rows and change.
- [X] T029-S1 Prove `cd web && nix develop --quiet -c just ci` plus the Playwright smoke command from `plan.md`.
- [X] T029-S1 Commit with subject `feat: add interactive coin selection ui` and trailer `Tasks: T029-S1`.

## Slice 2: Require the browser smoke in gate.sh

- [ ] T029-S2 Orchestrator-owned: extend `gate.sh` to derive Playwright paths from Nix and run `web/smoke/interactive-ui.mjs` after web CI.
- [ ] T029-S2 Orchestrator-owned: prove `./gate.sh`.
- [ ] T029-S2 Orchestrator-owned: commit with subject `chore: require interactive smoke in gate` and trailer `Tasks: T029-S2`.

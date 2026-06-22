# Tasks: SPA interactive coin-selection UI

## Slice 1: Interactive UI and browser smoke

- [X] T029-S1 Replace the fixed S2 output view with an editable Halogen input area seeded with the default text contract.
- [X] T029-S1 Validate local text input before calling the wasm and display clear on-page errors for empty or malformed input.
- [X] T029-S1 Run the existing `runCoinSelect` FFI on initial load and on submit, parse stdout into selected rows plus change, and render the structured result.
- [X] T029-S1 Update the plain CSS in `web/dist/index.html` for the textarea, action row, status/error display, result table, and responsive layout.
- [X] T029-S1 Add `web/smoke/interactive-ui.mjs` using Nix-provided Playwright to edit the input and assert the recomputed selected rows and change.
- [X] T029-S1 Prove `cd web && nix develop --quiet -c just ci` plus the Playwright smoke command from `plan.md`.
- [X] T029-S1 Commit with subject `feat: add interactive coin selection ui` and trailer `Tasks: T029-S1`.

## Slice 2: Structured UX presets and selected-row highlighting

- [X] T029-S2 Replace the raw textarea input with structured editable UTxO rows containing id and lovelace fields.
- [X] T029-S2 Add controls to add and remove UTxO rows without changing the wasm boundary contract.
- [X] T029-S2 Add a structured target amount field and build `utxo ...` plus `output ...` text under the hood before calling `runCoinSelect`.
- [X] T029-S2 Add preset scenario buttons for many small UTxOs, one big UTxO, and near-exact match; applying a preset repopulates inputs and runs selection.
- [X] T029-S2 Map `selected <id>` output lines back onto the UTxO pool and visibly mark/highlight selected pool rows.
- [X] T029-S2 Show selected total, target, and change totals; accept `change 0` for exact matches.
- [X] T029-S2 Update `web/smoke/interactive-ui.mjs` to exercise a preset plus an edit and assert selected-row highlighting plus totals.
- [X] T029-S2 Prove `cd web && nix develop --quiet -c just ci` plus the Playwright smoke command from `plan.md`.
- [X] T029-S2 Commit with subject `feat: make coin selection choices visible` and trailer `Tasks: T029-S2`.

## Slice 3: Require the browser smoke in gate.sh

- [X] T029-S3 Orchestrator-owned: extend `gate.sh` to derive Playwright paths from Nix and run `web/smoke/interactive-ui.mjs` after web CI.
- [X] T029-S3 Orchestrator-owned: prove `./gate.sh`.
- [X] T029-S3 Orchestrator-owned: commit with subject `chore: require interactive smoke in gate` and trailer `Tasks: T029-S3`.

# Specification: SPA interactive coin-selection UI

## User Story

As a visitor to the browser demo planned by parent epic #25, I need to edit a
UTxO set and target output directly in the PureScript SPA and recompute the
selection in my browser, so I can inspect the Haskell wasm coin-selection
behavior without a backend or a fixed example.

## Functional Requirements

- FR-001: The app must keep using the S1/S2 text-line wasm contract:
  `utxo <id> <lovelace>` and `output <addr> <lovelace>`.
- FR-002: The input area must be structured as an editable UTxO pool: each row
  has an input id and lovelace amount, with controls to add and remove rows.
- FR-003: The target output must be a separate editable amount field; the
  address may remain a fixed/cosmetic value because the selection behavior is
  amount-driven.
- FR-004: Preset scenario buttons must populate useful examples, including many
  small UTxOs, one big UTxO, and a near-exact or exact-match case.
- FR-005: The user must be able to submit the structured input and trigger
  `globalThis.runCoinSelect(stdinText)` through the existing PureScript FFI,
  with the same text contract built under the hood.
- FR-006: The UI must make the algorithm visible by mapping `selected <id>`
  output lines back onto the UTxO pool rows and highlighting/marking the rows
  the algorithm chose.
- FR-007: The result area must show change plus simple totals: selected total,
  target amount, and change.
- FR-008: The UI must still render selected inputs and change as structured,
  readable output instead of only dumping raw stdout.
- FR-009: Empty input, malformed local input, empty wasm output, and wasm
  error output must be shown as clear errors in the page.
- FR-010: The default example must still run on initial load so the page is not
  blank.
- FR-011: The implementation must use Halogen and plain CSS only. It must not
  add MUI, Emotion, React, UI packages, CDN scripts, or new runtime npm
  dependencies.
- FR-012: The npm runtime package budget must remain `<= 1`, with no runtime
  install scripts.
- FR-013: The branch gate must run web `just ci` and a browser smoke that uses a
  preset, edits a UTxO amount, runs selection, and asserts highlighted selected
  rows plus totals.

## Constraints

- S4 owns GitHub Pages deployment and CI deployment wiring. This ticket must not
  add Pages workflows.
- The wasm executable is generated locally into `web/src/assets/coin-select.wasm`
  and must remain untracked.
- The browser smoke may use Nix-provided Playwright tooling, but must not add
  Playwright to `web/package.json`.

## Acceptance Criteria

- Editing structured UTxO rows and the target amount recomputes the selection
  fully client-side through the wasm.
- Preset scenario buttons repopulate the UTxO pool and target in one click.
- Selected UTxO pool rows are visibly marked after selection, and totals show
  selected total, target, and change.
- Invalid or empty input surfaces an on-page error without crashing the app.
- `cd web && nix develop --quiet -c just ci` passes.
- `./gate.sh` passes and includes the structured-row/preset Playwright smoke.
- The runtime npm budget remains unchanged at one package:
  `@bjorn3/browser_wasi_shim`.

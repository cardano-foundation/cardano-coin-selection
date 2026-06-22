# Specification: SPA interactive coin-selection UI

## User Story

As a visitor to the browser demo planned by parent epic #25, I need to edit a
UTxO set and target output directly in the PureScript SPA and recompute the
selection in my browser, so I can inspect the Haskell wasm coin-selection
behavior without a backend or a fixed example.

## Functional Requirements

- FR-001: The app must keep using the S1/S2 text-line wasm contract:
  `utxo <id> <lovelace>` and `output <addr> <lovelace>`.
- FR-002: The input area must be editable and seeded with a sensible default
  example.
- FR-003: The user must be able to submit the edited input and trigger
  `globalThis.runCoinSelect(stdinText)` through the existing PureScript FFI.
- FR-004: The UI must render selected inputs and change as structured,
  readable output instead of only dumping raw stdout.
- FR-005: Empty input, malformed local input, empty wasm output, and wasm
  error output must be shown as clear errors in the page.
- FR-006: The default example must still run on initial load so the page is not
  blank.
- FR-007: The implementation must use Halogen and plain CSS only. It must not
  add MUI, Emotion, React, UI packages, CDN scripts, or new runtime npm
  dependencies.
- FR-008: The npm runtime package budget must remain `<= 1`, with no runtime
  install scripts.
- FR-009: The branch gate must run web `just ci` and a browser smoke that edits
  the input and asserts recomputed selected inputs plus change.

## Constraints

- S4 owns GitHub Pages deployment and CI deployment wiring. This ticket must not
  add Pages workflows.
- The wasm executable is generated locally into `web/src/assets/coin-select.wasm`
  and must remain untracked.
- The browser smoke may use Nix-provided Playwright tooling, but must not add
  Playwright to `web/package.json`.

## Acceptance Criteria

- Editing the input text and submitting recomputes the selection fully
  client-side through the wasm.
- The output view shows selected rows and the change amount for both the default
  input and an edited input.
- Invalid or empty input surfaces an on-page error without crashing the app.
- `cd web && nix develop --quiet -c just ci` passes.
- `./gate.sh` passes and includes the interactive Playwright smoke.
- The runtime npm budget remains unchanged at one package:
  `@bjorn3/browser_wasi_shim`.

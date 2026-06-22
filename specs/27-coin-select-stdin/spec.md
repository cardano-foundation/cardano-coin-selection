# Specification: JSON stdin coin selection executable

## User Story

As the browser SPA planned by parent epic #25, I need a wasm-compatible
coin-selection executable that accepts user-provided UTxO and target output
data on stdin, so the SPA can run the existing deterministic selection logic
without relying on the current hardcoded smoke fixture.

## Functional Requirements

- FR-001: The project must provide an executable named `coin-select`.
- FR-002: `coin-select` must read a JSON document from stdin with this stable
  shape:

  ```json
  {
    "utxos": [
      { "id": "input-1", "lovelace": 2000000 }
    ],
    "outputs": [
      { "address": "target-address", "lovelace": 4500000 }
    ]
  }
  ```

- FR-003: `coin-select` must construct `SelectionParams` equivalent to the
  existing `coin-select-smoke` fixture, except that UTxOs and outputs come from
  stdin.
- FR-004: Selection must run in `NonRandom` with `SelectionStrategyMinimal`.
- FR-005: Successful output must be deterministic and byte-identical between
  native and wasm runs for the same input. The required result shape is compact
  JSON:

  ```json
  {
    "selectedInputs": [
      { "id": "input-1", "lovelace": 2000000 }
    ],
    "change": [
      { "lovelace": 500000 }
    ]
  }
  ```

- FR-006: Parse or selection failure must exit non-zero and print a concise
  diagnostic to stderr.
- FR-007: The executable must build natively and with
  `wasm32-wasi-cabal --project-file=cabal-wasm.project build exe:coin-select`.
- FR-008: The repository must expose a lowercase `just ci` command while
  preserving the existing `CI` recipe.
- FR-009: The GitHub `wasm` job must build and run `coin-select` in addition
  to the existing hardcoded smoke executable.

## Constraints

- JSON is the planned SPA contract. If adding `aeson` proves incompatible with
  wasm or materially hostile to the wasm dependency surface, stop and write a
  parent Q-file before switching to a text format.
- Keep the library API focused on coin-selection primitives; CLI parsing and
  rendering should stay in the executable layer unless tests require a narrowly
  justified helper.
- The native and wasm sample output must be compared with `diff -u`.

## Acceptance Criteria

- `echo '<sample-json>' | cabal run exe:coin-select` prints the deterministic
  selection JSON.
- `echo '<sample-json>' | wasmtime coin-select.wasm` prints byte-identical
  output.
- `./gate.sh` passes at branch HEAD.
- Pull request #32 is ready for review only after GitHub checks `build`,
  `docs`, and `wasm` are green.

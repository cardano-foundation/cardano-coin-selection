# Specification: text stdin coin selection executable

## User Story

As the browser SPA planned by parent epic #25, I need a wasm-compatible
coin-selection executable that accepts user-provided UTxO and target output
data on stdin, so the SPA can run the existing deterministic selection logic
without relying on the current hardcoded smoke fixture.

## Functional Requirements

- FR-001: The project must provide an executable named `coin-select`.
- FR-002: `coin-select` must read a line-oriented text document from stdin
  with this stable shape:

  ```text
  utxo input-1 2000000
  output target-address 4500000
  ```

  Each non-empty line has space-separated fields. UTxO ids and addresses must
  not contain whitespace. Lovelace fields must be non-negative base-10
  integers.

- FR-003: `coin-select` must construct `SelectionParams` equivalent to the
  existing `coin-select-smoke` fixture, except that UTxOs and outputs come from
  stdin.
- FR-004: Selection must run in `NonRandom` with `SelectionStrategyMinimal`.
- FR-005: Successful output must be deterministic and byte-identical between
  native and wasm runs for the same input. The required output shape is:

  ```text
  selected input-1 2000000
  change 500000
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

- Use the text contract above to keep the wasm dependency surface lean. Do not
  add JSON dependencies such as `aeson` for this executable.
- Keep the library API focused on coin-selection primitives; CLI parsing and
  rendering should stay in the executable layer unless tests require a narrowly
  justified helper.
- The native and wasm sample output must be compared with `diff -u`.

## Acceptance Criteria

- `printf '<sample-text>' | cabal run exe:coin-select` prints the deterministic
  selection text.
- `printf '<sample-text>' | wasmtime coin-select.wasm` prints byte-identical
  output.
- `./gate.sh` passes at branch HEAD.
- Pull request #32 is ready for review only after GitHub checks `build`,
  `docs`, and `wasm` are green.

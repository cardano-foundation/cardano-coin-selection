# Plan: SPA interactive coin-selection UI

## Technical Approach

Build on the S2 Halogen scaffold and the Slice 1 interactive result view in
`web/src/Main.purs`. Replace the raw text input with structured editable state:
a UTxO pool (`id`, `lovelace`) plus a target amount. Build the same S1/S2 text
contract under the hood before calling `FFI.CoinSelect.runCoinSelect`.

Parse two boundaries explicitly:

- Local input validation checks that there is at least one UTxO row, that ids
  are non-empty, that UTxO amounts and target are positive integers, and that
  duplicate ids are rejected before the wasm call.
- Wasm stdout parsing accepts only `selected <id> <lovelace>` and
  `change <lovelace>` lines. Empty stdout, unknown lines, or stderr/error text
  becomes an on-page error. `change 0` is valid for exact matches.

Render the editable UTxO pool as a compact table/list. After selection, map the
parsed `selected` ids back to those rows and visibly mark each selected row. The
result area remains structured: selected inputs table, selected total, target,
and change. Use the existing `web/dist/index.html` embedded
stylesheet for plain CSS. Keep controls utilitarian: preset buttons, row
inputs, add/remove controls, target amount, run button, status text, error
area, result table.

Add three preset scenario buttons:

- Many small UTxOs
- One big UTxO
- Near-exact match

Each preset replaces the pool and target, then runs selection so the selected
rows and totals update immediately.

Keep `web/smoke/interactive-ui.mjs`, but update it for the structured UX. The
script should serve `web/dist`, import Nix-provided Playwright from
`${PLAYWRIGHT_CORE_PATH}/index.mjs`, click the "One big UTxO" preset, edit its
amount, run selection, and assert the highlighted selected row plus selected
total/target/change. It should also click the "Near-exact match" preset and
assert a zero-change result so the stdout parser allows `change 0`.

Do not add Playwright to `package.json`. The gate will derive
`PLAYWRIGHT_CORE_PATH` and `PLAYWRIGHT_BROWSERS_PATH` from Nix.

## Files

- `web/src/Main.purs` owns the Halogen state, structured validation, text
  contract generation, wasm call, stdout parsing, selected-row mapping, totals,
  presets, and rendering.
- `web/dist/index.html` owns the static shell and plain CSS.
- `web/smoke/interactive-ui.mjs` owns the browser smoke.
- `gate.sh` is parent-owned and is extended after the smoke exists.
- `specs/29-spa-interactive-ui/*` are parent-owned planning artifacts.

## Slice Breakdown

### Slice 1: Interactive UI and browser smoke

One driver+navigator slice updates the Halogen app, CSS, and smoke script. It
does not edit `gate.sh`; the focused proof is:

```sh
cd web && nix develop --quiet -c just ci
PLAYWRIGHT_CORE_PATH=$(nix build --no-link --print-out-paths nixpkgs#playwright-driver) \
PLAYWRIGHT_BROWSERS_PATH=$(nix build --no-link --print-out-paths nixpkgs#playwright-driver.browsers) \
nix shell nixpkgs#nodejs_22 -c node smoke/interactive-ui.mjs
```

### Slice 2: Structured UX presets and selected-row highlighting

One driver+navigator slice applies the UX refinement from coin-selection demo
research. It keeps the Slice 1 structured result display but replaces the raw
textarea with editable rows, target amount, preset scenarios, selected-row
highlighting, and totals. It also updates the Playwright smoke to exercise a
preset plus an edit.

Focused proof:

```sh
cd /code/cardano-coin-selection-issue-29/web && nix develop --quiet -c just ci
PLAYWRIGHT_CORE_PATH=$(nix build --no-link --print-out-paths nixpkgs#playwright-driver) \
PLAYWRIGHT_BROWSERS_PATH=$(nix build --no-link --print-out-paths nixpkgs#playwright-driver.browsers) \
nix shell nixpkgs#nodejs_22 -c node /code/cardano-coin-selection-issue-29/web/smoke/interactive-ui.mjs
```

If structured rows unexpectedly become too large, the worker must implement
presets first and write a Q-file before splitting the structured rows into
another slice.

### Slice 3: Require the browser smoke in gate.sh

Orchestrator-owned slice. After Slice 2 is verified, extend `gate.sh` to run
the same Playwright smoke after `cd web && nix develop --quiet -c just ci`.
Run `./gate.sh`, commit the gate change, then proceed to finalization.

## Verification

- Focused web proof: `cd web && nix develop --quiet -c just ci`.
- Browser proof: the Playwright smoke exercises a preset, edits a structured
  row, observes selected-row highlighting plus totals, then exercises the
  near-exact match preset and observes zero change.
- Full branch proof: `./gate.sh`.
- Budget proof remains the existing `jq` package count in `web/justfile` and
  `gate.sh`; expected runtime count is `1`.

## Risks

- The issue body mentions JSON stdin, but S1/S2 landed a text-line contract and
  the parent brief for this ticket makes the text contract authoritative.
- Browser smoke tooling is intentionally outside `web/package.json`; the smoke
  must fail clearly if `PLAYWRIGHT_CORE_PATH` or `PLAYWRIGHT_BROWSERS_PATH` is
  missing.
- The tracked root `WIP.md` predates this ticket. This ticket's live protocol is
  under `/tmp/epic-25/cardano-coin-selection-29/` and worker STATUS files.
- The Slice 1 v1 parser treated all lovelace outputs as positive. The Slice 2
  exact-match preset must make `change 0` valid while still rejecting nonpositive
  UTxO and target inputs.

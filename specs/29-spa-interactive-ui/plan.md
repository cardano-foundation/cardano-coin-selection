# Plan: SPA interactive coin-selection UI

## Technical Approach

Build on the S2 Halogen scaffold in `web/src/Main.purs`. Replace the fixed
`pre` input display with a controlled textarea seeded from the existing default
text contract. Keep the state small: current input text, running/error status,
and parsed result. On initialization and on submit, run the existing
`FFI.CoinSelect.runCoinSelect` function with the textarea contents.

Parse two boundaries explicitly:

- Local input validation checks that at least one `utxo` line and one `output`
  line exist, that every line has the expected token count, and that lovelace
  fields are positive integers.
- Wasm stdout parsing accepts only `selected <id> <lovelace>` and
  `change <lovelace>` lines. Empty stdout, unknown lines, or stderr/error text
  becomes an on-page error.

Render selected inputs in a compact table and render change as a separate
summary value. Preserve raw stdout as a secondary debug block only if it helps
implementation; the primary result must be structured. Use the existing
`web/dist/index.html` embedded stylesheet for plain CSS. Keep controls
utilitarian: textarea, run button, status text, error area, result table.

Add a browser smoke under `web/smoke/interactive-ui.mjs`. The script should
serve `web/dist`, import Nix-provided Playwright from
`${PLAYWRIGHT_CORE_PATH}/index.mjs`, edit the textarea, submit the form, and
assert the recomputed output for this sample:

```text
utxo input-a 1000000
utxo input-b 2500000
utxo input-c 4000000
output target-address 6000000
```

Expected selected lines:

```text
selected input-a 1000000
selected input-b 2500000
selected input-c 4000000
change 1500000
```

Do not add Playwright to `package.json`. The gate will derive
`PLAYWRIGHT_CORE_PATH` and `PLAYWRIGHT_BROWSERS_PATH` from Nix.

## Files

- `web/src/Main.purs` owns the Halogen state, validation, wasm call, stdout
  parsing, and rendering.
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

### Slice 2: Require the browser smoke in gate.sh

Orchestrator-owned slice. After Slice 1 is verified, extend `gate.sh` to run
the same Playwright smoke after `cd web && nix develop --quiet -c just ci`.
Run `./gate.sh`, commit the gate change, then proceed to finalization.

## Verification

- Focused web proof: `cd web && nix develop --quiet -c just ci`.
- Browser proof: the Playwright smoke edits the input and observes
  `selected input-a 1000000`, `selected input-b 2500000`,
  `selected input-c 4000000`, and `change 1500000`.
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

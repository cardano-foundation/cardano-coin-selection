# Plan: Pages deploy and CI for the SPA

## Approved Pages Strategy

Parent answer `A-001-pages-strategy` chose the conservative MkDocs strategy:
keep `.github/workflows/docs.yml` deploying the existing docs site to
`gh-pages`, and make the SPA a static subdirectory of that same site at
`/demo/`.

The implementation must not switch the repository to the GitHub Actions Pages
artifact source. The workflow should generate `docs/demo/` only inside CI (and
local gate scratch state), so the committed docs tree stays source-only.

## Technical Approach

Update the PR CI workflow with a dedicated `web` job on `ubuntu-latest`.
The job mirrors the existing wasm cache shape, then runs:

```sh
cd web && nix develop --quiet -c just ci
```

That existing recipe performs `npm ci`, `purs-tidy check`, `spago build`,
`prepare-wasm` when needed, bundle, runtime npm budget, and install-script
checks.

Update the docs deploy workflow to preserve the existing `mkdocs-deploy`
mechanism:

1. checkout and setup Nix with Cachix;
2. restore/cache ghc-wasm Cabal directories;
3. run `cd web && nix develop --quiet -c just ci` to build a fresh wasm-backed
   SPA bundle;
4. copy `web/dist/*` into `docs/demo/`;
5. run the existing MkDocs deploy command.

Extend `web/smoke/interactive-ui.mjs` so the same smoke can load the app from a
subpath. A `SMOKE_BASE_PATH=/demo/` environment variable is enough: the local
test server maps `/demo/` to the generated dist root, while defaulting to `/`
for existing root-level smoke usage.

After the driver slice, extend parent-owned `gate.sh` to:

- run the existing full gate;
- derive Nix-provided Playwright paths;
- smoke the app at `SMOKE_BASE_PATH=/demo/`;
- actionlint the changed workflow files.

## Files

- `.github/workflows/ci.yml` adds the PR web gate.
- `.github/workflows/docs.yml` adds wasm/web bundle/copy before MkDocs deploy.
- `web/smoke/interactive-ui.mjs` gains subpath smoke support.
- `gate.sh` is parent-owned and extended after the driver slice.
- `specs/30-pages-deploy/*` are parent-owned planning artifacts.

## Slice Breakdown

### Slice 1: CI/deploy wiring and subpath smoke support

Driver/navigator slice. Owned files:

- `.github/workflows/ci.yml`
- `.github/workflows/docs.yml`
- `web/smoke/interactive-ui.mjs`

Focused proof:

```sh
cd web && nix develop --quiet -c just ci
PLAYWRIGHT_CORE_PATH=$(nix build --no-link --print-out-paths nixpkgs#playwright-driver) \
PLAYWRIGHT_BROWSERS_PATH=$(nix build --no-link --print-out-paths nixpkgs#playwright-driver.browsers) \
SMOKE_BASE_PATH=/demo/ \
nix shell nixpkgs#nodejs_22 -c node smoke/interactive-ui.mjs
nix shell nixpkgs#actionlint --command actionlint -shellcheck= \
  .github/workflows/ci.yml \
  .github/workflows/docs.yml \
  .github/workflows/release.yml
```

The driver must not edit `gate.sh`, specs, Haskell source, PureScript source, or
npm manifests. If one of those files becomes necessary, write a Q-file.

### Slice 2: Parent-owned gate extension

Orchestrator-owned slice. Extend `gate.sh` with the `/demo/` Playwright smoke
and any workflow actionlint target adjustments needed after Slice 1.

Focused proof:

```sh
./gate.sh
```

Commit subject:

```text
chore: require pages demo smoke in gate

Tasks: T030-S2
```

## Verification

- Local branch proof: `./gate.sh`.
- Driver focused proof: web CI, `/demo/` Playwright smoke, actionlint.
- PR proof: GitHub checks on draft PR #35 must pass before `COMPLETE`.
- Live URL expectation: after merge to `main`, the docs deploy workflow publishes
  the SPA at
  `https://cardano-foundation.github.io/cardano-coin-selection/demo/`.

## Risks

- The real public URL cannot be fetched until after merge because the deploy
  runs on `push` to `main`.
- MkDocs must copy `docs/demo/index.html` and `docs/demo/index.js` as static
  files without trying to put them in nav. The local gate smoke against
  `/demo/` is the pre-merge proof.
- `actionlint`'s embedded shellcheck flags a pre-existing workflow snippet, so
  the gate uses `-shellcheck=` to validate workflow structure without expanding
  this ticket into unrelated shell cleanup.

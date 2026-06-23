# Plan: Browser WASM announcement

## Technical Approach

Add one MkDocs page focused on the public outcome of epic #25 rather than the
implementation mechanics. The page should answer four questions:

1. What was built: the real Cardano wallet coin-selection algorithm compiled to
   `wasm32-wasi` and run fully client-side in the browser.
2. Why it matters: the browser demo uses the same audited Haskell
   implementation as native users, avoiding a parallel JavaScript
   implementation.
3. How to try it: link the hosted `/demo/` page.
4. Where to get the artifact: link the v0.1.0 release with the WASM asset.

Keep the existing `Browser Demo` page as the hands-on page and the existing
`WASM Portability Audit` page as the engineering evidence page. The new page is
the public summary that links to both. Add small reciprocal links from those two
pages back to the summary so visitors can move between overview, demo, and
portability notes.

Put the ready-to-post draft under `announce/` rather than `docs/`. MkDocs strict
mode warns on docs files omitted from nav, while the announcement draft should
remain copy-ready repository content rather than a public docs navigation item.

## Files

- `docs/browser-wasm.md` for the public summary page.
- `docs/browser-demo.md` for a small reciprocal overview link.
- `docs/architecture/wasm-portability.md` for a small reciprocal overview link.
- `mkdocs.yml` to add the new page to the nav.
- `announce/wasm-browser-demo.md` for the ready-to-post draft announcement.
- `specs/31-announcement/*` are parent-owned planning artifacts.

## Slice Breakdown

### Slice 1: Public docs page and announcement draft

Driver/navigator slice. Owned files:

- `docs/browser-wasm.md`
- `docs/browser-demo.md`
- `docs/architecture/wasm-portability.md`
- `mkdocs.yml`
- `announce/wasm-browser-demo.md`

Focused RED proof:

```sh
test -f docs/browser-wasm.md && test -f announce/wasm-browser-demo.md
```

This command should fail before the slice creates the page and draft.

Focused GREEN proof:

```sh
test -f docs/browser-wasm.md && test -f announce/wasm-browser-demo.md
nix develop --quiet -c just docs-build
./gate.sh
```

The driver must not edit Haskell source, PureScript source, workflows, release
metadata, `gate.sh`, or the `specs/31-announcement/*` contract files.

Commit subject:

```text
docs: add browser wasm announcement

Tasks: T031-S1
```

## Verification

- Local branch proof: `./gate.sh`.
- Content proof: the docs page and announcement draft include both the live demo
  URL and the v0.1.0 release URL.
- PR proof: GitHub checks on draft PR #37 must pass before `COMPLETE`.

## Risks

- Public announcement wording can easily become promotional. Keep it factual:
  shipped capability, why a single implementation matters, demo link, release
  link.
- The announcement draft must not be accidentally placed in an external channel;
  repository content only.

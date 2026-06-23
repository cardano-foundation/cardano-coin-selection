# Specification: Browser WASM announcement

## User Story

As a project visitor, I need a concise public summary of the browser-runnable
WASM coin-selection work, so I can understand what was built, try the live
demo, and find the v0.1.0 release without reading the implementation tickets.

As the project maintainer, I need a ready-to-post announcement draft in the
repository, so I can publish the same factual message to a forum or Discord
without asking the PR author to post externally.

## Functional Requirements

- FR-001: Add a public-facing documentation page that explains that the real
  Cardano wallet coin-selection algorithm, extracted from `cardano-wallet`, now
  runs in the browser through `wasm32-wasi`.
- FR-002: The page must explain the reason for the work: one audited Haskell
  implementation usable natively, in WASM, and in the browser, instead of a
  JavaScript reimplementation.
- FR-003: The page must link the live demo:
  `https://cardano-foundation.github.io/cardano-coin-selection/demo/`.
- FR-004: The page must link the v0.1.0 release:
  `https://github.com/cardano-foundation/cardano-coin-selection/releases/tag/v0.1.0`.
- FR-005: The page must cross-link the existing browser demo page and the WASM
  portability audit.
- FR-006: The new docs page must be included in `mkdocs.yml` navigation.
- FR-007: Add a ready-to-post announcement draft in the repository, outside any
  external channel, with the demo and release links.
- FR-008: The announcement tone must be short, factual, low-profile, and must
  not include AI attribution.

## Constraints

- No external posting is part of this ticket.
- Do not change the Haskell library, PureScript SPA, release assets, CI deploy
  behavior, or dependency manifests.
- `mkdocs build --strict` must stay green.
- The announcement draft is repository content, not PR body-only text, so it
  survives merge and can be copied later.

## Acceptance Criteria

- A MkDocs-nav documentation page summarizes the WASM/browser capability and
  links the live demo and v0.1.0 release.
- The docs page links both `docs/browser-demo.md` and
  `docs/architecture/wasm-portability.md`.
- A ready-to-post announcement draft exists in the repository and is not posted
  externally.
- `./gate.sh` passes after the content lands.

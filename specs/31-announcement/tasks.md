# Tasks: Browser WASM announcement

## Slice 1: Public docs page and announcement draft

- [ ] T031-S1 Add `docs/browser-wasm.md` summarizing the browser-runnable WASM
  coin-selection capability and linking the live demo and v0.1.0 release.
- [ ] T031-S1 Add the new summary page to `mkdocs.yml` navigation.
- [ ] T031-S1 Cross-link `docs/browser-demo.md` and
  `docs/architecture/wasm-portability.md` with the new summary page.
- [ ] T031-S1 Add `announce/wasm-browser-demo.md` as a ready-to-post,
  low-profile announcement draft with the demo and release links.
- [ ] T031-S1 Prove the focused RED command fails before content creation, then
  prove `nix develop --quiet -c just docs-build` and `./gate.sh` pass.
- [ ] T031-S1 Commit with subject `docs: add browser wasm announcement` and
  trailer `Tasks: T031-S1`.

## Finalization

- [ ] T031-F1 Verify all task boxes are checked in this file.
- [ ] T031-F1 Verify PR #37 GitHub checks are green.
- [ ] T031-F1 Update the PR body with delivered files and validation evidence.
- [ ] T031-F1 Drop `gate.sh` in the final ready-for-review commit.
- [ ] T031-F1 Mark PR #37 ready for review only after checks are green.

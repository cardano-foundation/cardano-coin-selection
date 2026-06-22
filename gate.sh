#!/usr/bin/env bash
set -euo pipefail

echo "==> git diff --check"
git diff --check

echo "==> native CI"
# The repo's existing Just recipe is named "CI" rather than "ci".
nix develop --quiet -c just CI

echo "==> wasm ram dependency build"
nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12' \
  --command bash -lc \
  'wasm32-wasi-cabal --project-file=cabal-wasm.project build ram'

echo "==> wasm library build (expected int-cast blocker for #20)"
wasm_log=$(mktemp)
trap 'rm -f "$wasm_log"' EXIT
set +e
nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12' \
  --command bash -lc \
  'wasm32-wasi-cabal --project-file=cabal-wasm.project build lib:cardano-coin-selection' \
  2>&1 | tee "$wasm_log"
wasm_status=${PIPESTATUS[0]}
set -e

if [ "$wasm_status" -eq 0 ]; then
  echo "WASM library build completed; #20 blocker is already absent."
  exit 0
fi

if grep -q 'Failed to build ram-' "$wasm_log"; then
  echo "WASM build failed before/past ram proof: ram failed to build." >&2
  exit 1
fi

if grep -q 'Failed to build int-cast-0.2.0.0' "$wasm_log" \
  && grep -q 'HTYPE_SIG_ATOMIC_T' "$wasm_log"; then
  echo "WASM build reached the expected #20 blocker: int-cast HTYPE_SIG_ATOMIC_T."
  exit 0
fi

echo "WASM build failed for an unexpected reason; expected only int-cast HTYPE_SIG_ATOMIC_T." >&2
exit 1

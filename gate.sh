#!/usr/bin/env bash
set -euo pipefail

echo "==> git diff --check"
git diff --check

echo "==> native CI"
# The repo's existing Just recipe is named "CI" rather than "ci".
nix develop --quiet -c just CI

echo "==> wasm library build"
nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12' \
  --command bash -lc \
  'wasm32-wasi-cabal --project-file=cabal-wasm.project build lib:cardano-coin-selection'

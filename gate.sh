#!/usr/bin/env bash
set -euo pipefail

echo "== git diff whitespace check =="
git diff --check

echo "== native CI =="
nix develop --quiet -c just CI

echo "== wasm library build =="
if command -v wasm32-wasi-cabal >/dev/null 2>&1; then
  wasm32-wasi-cabal --project-file=cabal-wasm.project build lib:cardano-coin-selection
else
  nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12' \
    --command bash -c 'wasm32-wasi-cabal --project-file=cabal-wasm.project build lib:cardano-coin-selection'
fi

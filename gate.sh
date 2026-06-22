#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

git diff --check

nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12' --command bash -c '
  set -euo pipefail
  wasm32-wasi-cabal update
  wasm32-wasi-cabal --project-file=cabal-wasm.project build exe:coin-select
  mkdir -p web/src/assets
  wasm=$(find dist-newstyle -name coin-select.wasm -type f | head -1)
  test -n "$wasm"
  cp "$wasm" web/src/assets/coin-select.wasm
'

(
  cd web
  nix develop --quiet -c just ci

  runtime_count=$(
    jq '[.packages | to_entries[] | select(.key != "" and (.value.dev != true))] | length' package-lock.json
  )
  if [ "$runtime_count" -gt 1 ]; then
    printf 'npm runtime package budget exceeded: %s > 1\n' "$runtime_count" >&2
    exit 1
  fi

  install_script_count=$(
    jq '[.packages | to_entries[] | select((.value.dev != true) and .value.hasInstallScript == true)] | length' package-lock.json
  )
  if [ "$install_script_count" -ne 0 ]; then
    printf 'runtime package install scripts present: %s\n' "$install_script_count" >&2
    exit 1
  fi
)

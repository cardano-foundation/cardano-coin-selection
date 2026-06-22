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

  playwright_core_path=$(nix build --no-link --print-out-paths nixpkgs#playwright-driver)
  playwright_browsers_path=$(nix build --no-link --print-out-paths nixpkgs#playwright-driver.browsers)
  PLAYWRIGHT_CORE_PATH="$playwright_core_path" \
    PLAYWRIGHT_BROWSERS_PATH="$playwright_browsers_path" \
    nix shell nixpkgs#nodejs_22 -c node smoke/interactive-ui.mjs

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

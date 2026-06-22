#!/usr/bin/env bash
set -euo pipefail

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

cat >"$tmp/input.json" <<'JSON'
{"utxos":[{"id":"input-1","lovelace":2000000},{"id":"input-2","lovelace":3000000},{"id":"input-3","lovelace":5000000}],"outputs":[{"address":"target-address","lovelace":4500000}]}
JSON

git diff --check
nix develop --quiet -c just ci

nix develop --quiet -c bash -c '
  set -euo pipefail
  cabal build -O0 exe:coin-select
  exe=$(cabal list-bin -O0 exe:coin-select)
  "$exe" < "$1"
' bash "$tmp/input.json" >"$tmp/native.out"

SAMPLE_INPUT="$tmp/input.json" nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12' --command bash -c '
  set -euo pipefail
  wasm32-wasi-cabal update
  wasm32-wasi-cabal --project-file=cabal-wasm.project build exe:coin-select
  wasm=$(find dist-newstyle -name "coin-select.wasm" -type f | head -1)
  test -n "$wasm"
  wasmtime "$wasm" < "$SAMPLE_INPUT"
' >"$tmp/wasm.out"

diff -u "$tmp/native.out" "$tmp/wasm.out"

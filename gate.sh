#!/usr/bin/env bash
set -euo pipefail

tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/ccs-smoke-gate.XXXXXX")
cleanup() {
    rm -rf "$tmp_dir"
}
trap cleanup EXIT

git diff --check
nix develop --quiet -c just CI

if ! grep -q '^executable coin-select-smoke' cardano-coin-selection.cabal; then
    echo "coin-select-smoke executable not present yet; smoke checks skipped"
    exit 0
fi

if [ -d app ]; then
    nix develop --quiet -c fourmolu -m check app
    nix develop --quiet -c hlint app
fi

nix develop --quiet -c cabal run -O0 exe:coin-select-smoke >"$tmp_dir/native.out"

nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12' \
    --command bash -c '
        set -euo pipefail
        wasm32-wasi-cabal --project-file=cabal-wasm.project build exe:coin-select-smoke >&2
        wasm=$(find dist-newstyle -name "coin-select-smoke.wasm" -type f | head -1)
        test -n "$wasm"
        wasmtime "$wasm"
    ' >"$tmp_dir/wasm.out"

diff -u "$tmp_dir/native.out" "$tmp_dir/wasm.out"

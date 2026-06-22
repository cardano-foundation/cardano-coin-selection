#!/usr/bin/env bash
set -euo pipefail

tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/ccs-wasm-ci-gate.XXXXXX")
cleanup() {
    rm -rf "$tmp_dir"
}
trap cleanup EXIT

git diff --check
nix shell 'nixpkgs#actionlint' --command actionlint .github/workflows/ci.yml
nix develop --quiet -c just CI

nix develop --quiet -c bash -c '
    set -euo pipefail
    cabal build -O0 exe:coin-select-smoke >&2
    exe=$(cabal list-bin -O0 exe:coin-select-smoke)
    "$exe"
' >"$tmp_dir/native.out"

nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12' \
    --command bash -c '
        set -euo pipefail
        wasm32-wasi-cabal --project-file=cabal-wasm.project build exe:coin-select-smoke >&2
        wasm=$(find dist-newstyle -name "coin-select-smoke.wasm" -type f | head -1)
        test -n "$wasm"
        wasmtime "$wasm"
    ' >"$tmp_dir/wasm.out"

diff -u "$tmp_dir/native.out" "$tmp_dir/wasm.out"

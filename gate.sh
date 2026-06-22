#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

git diff --check
nix develop --quiet -c just ci
nix develop --quiet -c just docs-build
(
  cd web
  nix develop --quiet -c just ci
)
nix shell nixpkgs#actionlint --command actionlint -shellcheck= \
  .github/workflows/ci.yml \
  .github/workflows/docs.yml \
  .github/workflows/release.yml

#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

git diff --check
nix develop --quiet -c just ci
(
  cd web
  nix develop --quiet -c just ci
  PLAYWRIGHT_CORE_PATH=$(nix build --no-link --print-out-paths nixpkgs#playwright-driver) \
  PLAYWRIGHT_BROWSERS_PATH=$(nix build --no-link --print-out-paths nixpkgs#playwright-driver.browsers) \
  SMOKE_BASE_PATH=/demo/ \
  nix shell nixpkgs#nodejs_22 -c node smoke/interactive-ui.mjs
)
rm -rf docs/demo
trap 'rm -rf docs/demo' EXIT
mkdir -p docs/demo
cp -R web/dist/. docs/demo/
test -f docs/demo/index.html
test -f docs/demo/index.js
nix develop --quiet -c just docs-build
nix shell nixpkgs#actionlint --command actionlint -shellcheck= \
  .github/workflows/ci.yml \
  .github/workflows/docs.yml \
  .github/workflows/release.yml

#!/usr/bin/env bash
set -euo pipefail

git diff --check
nix develop github:paolino/dev-assets?dir=mkdocs -c mkdocs build --strict

#!/usr/bin/env bash
set -euo pipefail

git diff --check
nix develop --quiet -c bash -lc 'just docs-build && just CI'

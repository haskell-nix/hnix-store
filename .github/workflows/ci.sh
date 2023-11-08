#!/usr/bin/env bash
# Script by @fisx

set -eo pipefail
cd "$( dirname "${BASH_SOURCE[0]}" )"

which dhall-to-yaml || cabal install dhall-yaml
echo "regenerating ci.yaml"
dhall-to-yaml-ng --generated-comment --file ci.dhall > ci.yaml

#!/usr/bin/env bash
# Script by @fisx

set -eo pipefail
cd "$( dirname "${BASH_SOURCE[0]}" )"

which dhall || cabal install dhall
which dhall-to-yaml || cabal install dhall-yaml

echo "dhall format-ing ci.dhall"
dhall format ci.dhall
echo "cp haskellCi.dhall -> ci.dhall.frozen"
cp ci.dhall ci.dhall.frozen
echo "dhall freez-ing ci.dhall.frozen"
dhall freeze ci.dhall.frozen
echo "regenerating ci.yaml"
dhall-to-yaml-ng --generated-comment --file ci.dhall > ci.yaml

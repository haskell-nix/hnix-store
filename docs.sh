#!/usr/bin/env bash
set -e
set -x

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -rf "$dir"' EXIT

# assumes cabal 2.4 or later
cabal haddock --builddir="$dir" --haddock-for-hackage --enable-doc

cabal upload -d $@ $dir/*-docs.tar.gz

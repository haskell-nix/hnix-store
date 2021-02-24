#!/usr/bin/env bash
set -e
set -x

echo $@
# add --publish to really do it

rm -rf dist-newstyle

for i in core remote; do
  echo $i
  n=hnix-store-$i
  pushd $n

  rm -rf dist-newstyle

  cabal sdist
  cabal upload $@ dist-newstyle/sdist/$n-*.tar.gz

  # docs
  dir=$(mktemp -d dist-docs.XXXXXX)
  trap 'rm -rf "$dir"' EXIT

  # assumes cabal 2.4 or later
  cabal haddock --builddir="$dir" --haddock-for-hackage --enable-doc

  cabal upload -d $@ $dir/$n*-docs.tar.gz

  popd
done

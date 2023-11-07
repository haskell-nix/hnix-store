#!/usr/bin/env bash

# Generate a NAR file with case conflicts in the file names.
mkdir -p case-conflict/bar
touch case-conflict/{Foo.txt,foo.txt,Baz.txt,bar/baz.txt}

storePath=$(nix-store --add ./case-conflict)
nix-store --dump $storePath > case-conflict.nar

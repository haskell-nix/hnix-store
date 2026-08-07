# hnix-store-nar

`NAR` file format packing and unpacking.

For a description of the NAR format, see [`Eelco's thesis`](https://nixos.org/~eelco/pubs/phd-thesis.pdf).

The [NAR streaming benchmark](./benchmarks/README.md) provides a reproducible
Hyperfine comparison between the working tree and a baseline Git revision.

[System.Nix.Nar]: ./src/System/Nix/Nar.hs
[System.Nix.Nar.Effects]: ./src/System/Nix/Nar/Effects.hs

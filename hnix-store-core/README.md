hnix-store-core
=================

Core effects for interacting with the Nix store.

See `NarEffects` in [System.Nix.Internal.Nar.Effects] and the [System.Nix.StorePath] for the available operations
on the store.

[System.Nix.Internal.Nar.Effects]: ./src/System/Nix/Internal/Nar/Effects.hs
[System.Nix.StorePath]: ./src/System/Nix/StorePath.hs


Tests
======

 - `ghcid --command "cabal repl test-suite:format-tests" --test="Main.main"`

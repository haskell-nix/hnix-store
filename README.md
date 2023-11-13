# hnix-store

A Haskell interface to the [Nix] store.

[Nix]: https://nixos.org/nix

## Rationale

`Nix` can conceptually be broken up into two layers, both (perhaps
unfortunately) named "Nix": The expression language and the store.
The semantics of the expression language fundamentally depend on the
store, but the store is independent of the language. The store
semantics provide the basic building blocks of `Nix`:
content-addressed files and directories, the drv file format and the
semantics for building drvs, tracking references of store paths,
copying files between stores (or to/from caches), distributed builds,
etc.

The goal of `hnix-store` is to provide a Haskell interface to the Nix
store semantics, as well as various implementations of that interface.
Though the current primary client is [hnix], an effort to reimplement
the `Nix` expression language in Haskell, this project is meant to be
generic and could be used for a number of other cases of interaction
with the `Nix` store (e.g. a `shake` backend that emitted each build
action as a store derivation). Currently, there are three
implementations planned:

* A `mock` store which performs no IO whatsoever, for unit testing.
* A `readonly` store, which defers to another implementation for
  readonly effects (such as querying whether some path is valid in the
  store, or reading a file) but performs mutating effects in-memory
  only (for example, computing the store path a given directory would
  live at if added to the store, without actually modifying anything).
* A `remote` store, which implements the client side of the `Nix`
  daemon Unix domain socket protocol, allowing full interaction with
  the store on a system with the C++ daemon installed.

[hnix]: https://github.com/haskell-nix/hnix

## Packages

In the interest of separating concerns, this project is split into
several Haskell packages.

### [hnix-store-core]

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/haskell-nix/hnix-store/ci.yaml?branch=master)](https://github.com/haskell-nix/hnix-store/actions/workflows/ci.yaml)
[![Hackage version](https://img.shields.io/hackage/v/hnix-store-core.svg?color=success)](https://hackage.haskell.org/package/hnix-store-core)
[![Dependencies](https://img.shields.io/hackage-deps/v/hnix-store-core?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=hnix-store-core)

Contains the core effect types and
fundamental operations combining them, agnostic to any particular
effectful implementation (e.g. in-memory, talking to the Nix daemon in
IO, etc.), with the actual implementations in a different package.

The intent is that core business logic for a project that needs to
interact with the `Nix` store can simply depend on `hnix-store-core`,
and only at the very edges of the system would it be necessary to
bring in a specific implementation.

### [hnix-store-remote]

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/haskell-nix/hnix-store/ci.yaml?branch=master)](https://github.com/haskell-nix/hnix-store/actions/workflows/ci.yaml)
[![Hackage version](https://img.shields.io/hackage/v/hnix-store-remote.svg?color=success)](https://hackage.haskell.org/package/hnix-store-remote)
[![Dependencies](https://img.shields.io/hackage-deps/v/hnix-store-remote?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=hnix-store-remote)

[Nix] worker protocol implementation for interacting with remote Nix store
via `nix-daemon`.

[hnix-store-core]: ./hnix-store-core
[hnix-store-remote]: ./hnix-store-remote

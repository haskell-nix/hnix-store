# hnix-store

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/haskell-nix/hnix-store/ci.yaml?branch=master)](https://github.com/haskell-nix/hnix-store/actions/workflows/ci.yaml)

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

[![Hackage version](https://img.shields.io/hackage/v/hnix-store-core.svg?color=success)](https://hackage.haskell.org/package/hnix-store-core)
[![Dependencies](https://img.shields.io/hackage-deps/v/hnix-store-core?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=hnix-store-core)

Contains the core types and
fundamental operations, agnostic to any particular
effectful implementation (e.g. in-memory, talking to the Nix daemon in
IO, etc.), with the actual implementations in a different package.

The intent is that core business logic for a project that needs to
interact with the `Nix` store can simply depend on `hnix-store-core`,
and only at the very edges of the system would it be necessary to
bring in a specific implementation.

### [hnix-store-db]

[![Hackage version](https://img.shields.io/hackage/v/hnix-store-db.svg?color=success)](https://hackage.haskell.org/package/hnix-store-db)
[![Dependencies](https://img.shields.io/hackage-deps/v/hnix-store-db?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=hnix-store-db)

Implementation of the `Nix` store SQLite database.

### [hnix-store-json]

[![Hackage version](https://img.shields.io/hackage/v/hnix-store-json.svg?color=success)](https://hackage.haskell.org/package/hnix-store-json)
[![Dependencies](https://img.shields.io/hackage-deps/v/hnix-store-json?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=hnix-store-json)

`Aeson` instances for core types, required for remote store protocol.

### [hnix-store-nar]

[![Hackage version](https://img.shields.io/hackage/v/hnix-store-nar.svg?color=success)](https://hackage.haskell.org/package/hnix-store-nar)
[![Dependencies](https://img.shields.io/hackage-deps/v/hnix-store-nar?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=hnix-store-nar)

Packing and unpacking for NAR file format used by Nix.

### [hnix-store-readonly]

[![Hackage version](https://img.shields.io/hackage/v/hnix-store-readonly.svg?color=success)](https://hackage.haskell.org/package/hnix-store-readonly)
[![Dependencies](https://img.shields.io/hackage-deps/v/hnix-store-readonly?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=hnix-store-readonly)

Path computation without interaction with the actual `Nix` store

### [hnix-store-remote]

[![Hackage version](https://img.shields.io/hackage/v/hnix-store-remote.svg?color=success)](https://hackage.haskell.org/package/hnix-store-remote)
[![Dependencies](https://img.shields.io/hackage-deps/v/hnix-store-remote?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=hnix-store-remote)

[Nix] worker protocol implementation for interacting with remote Nix store
via `nix-daemon`.

### [hnix-store-tests]

[![Hackage version](https://img.shields.io/hackage/v/hnix-store-tests.svg?color=success)](https://hackage.haskell.org/package/hnix-store-tests)
[![Dependencies](https://img.shields.io/hackage-deps/v/hnix-store-tests?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=hnix-store-tests)

Aribtrary instances and utilities for testing.

[hnix-store-core]: ./hnix-store-core
[hnix-store-db]: ./hnix-store-db
[hnix-store-json]: ./hnix-store-json
[hnix-store-nar]: ./hnix-store-nar
[hnix-store-readonly]: ./hnix-store-readonly
[hnix-store-remote]: ./hnix-store-remote
[hnix-store-tests]: ./hnix-store-tests

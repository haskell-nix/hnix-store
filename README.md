hnix-store
===========

A Haskell interface to the [Nix] store.

[Nix]: https://nixos.org/nix

Rationale
-----------

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
* A `daemon` store, which implements the client side of the `Nix`
  daemon Unix domain socket protocol, allowing full interaction with
  the store on a system with the C++ daemon installed.

While this project is in the early stages of development, the `daemon`
store can be seen as something of a road map: We want to express and
implement all of (and only) the useful functionality available to a
client of the existing daemon protocol.

Note that there are currently no plans for hnix-store to include an
implementation which directly mutates the filesystem and database of
the `Nix` store.

[hnix]: https://github.com/haskell-nix/hnix

Packages
----------

In the interest of separating concerns, this project is split into
several Haskell packages. The current expectation is at least one
package, [hnix-store-core], containing the core effect types and
fundamental operations combining them, agnostic to any particular
effectful implementation (e.g. in-memory, talking to the Nix daemon in
IO, etc.), with the actual implementations in a different package.
Whether each implementation gets its own package or not remains to be
seen.

The intent is that core business logic for a project that needs to
interact with the `Nix` store can simply depend on `hnix-store-core`,
and only at the very edges of the system would it be necessary to
bring in a specific implementation.

[hnix-store-core]: ./hnix-store-core

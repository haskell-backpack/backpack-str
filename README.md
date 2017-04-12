# backpack-str

This repository defines a collection of packages which
operate on strings (monomorphic sequences, if you like).
These packages can be categorized in the following ways:

## Implementation packages

The packages `str-string`, `str-text`, `str-bytestring` and
`str-foundation` provide *uniform*, monomorphic interfaces to all of the
functionality offered by these packages, also filling in missing
functionality.  You can transparently swap out one package for another,
and as long as your code is using functions were are common to both
packages, you can be assured the types of the identifiers do not change.
These packages smooth over a number of minor API differences between
these packages.

## Signature packages

The `str-sig` package provides a Backpack signature (GHC 8.2 only) that
specifies the "full" abstract interface for strings (monomorphic
sequences), abstracting over `Str` (the string type), `Chr` (the
character type) and `Index` (the numeric type for indexing into strings;
either `Int` or `Int64`); `str-sig` records the union of all functions
supported by every package in this repository.

For more information on how to effectively use this package, see the
[README](str-sig/README.md) for str-sig.

## "Partial" implementation packages

The `str-string-partial`, `str-text-partial`, `str-bytestring-partial`
and `str-foundation-partial` packages provide modules which implement
the *full* set of types and functions specified by `str-sig`, unlike
their ordinary counterparts which may be missing functionality.  (They
are called "partial" because any missing functionality will raise a
runtime error when used.)

In general, you shouldn't use directly depend on these packages, as
they are less type-safe than their total counterparts.  But you might
find them useful in the following situations:

1. You are writing code which can gracefully handle the "missing
   functionality" runtime exception; an example is a test suite which
   skips a test when such an exception is raised.

2. For temporarily running code parametrized over `str-sig` when you
   haven't thinned the signature yet (but really you should!)

For internal development purposes, these packages are also used to
verify that the corresponding implementation modules actually implement
`str-sig`.

There is also a `str-undefined` package which assists in implementing
partial packages by providing a module that defines placeholder
implementations for all of `str-sig`.  The idea is that, after
instantiating `str-undefined` to the correct type, you can
pick between exporting an entity from `str-undefined` or the
actual implementation module when defining the partial variant of
the module.  We take advantage of the fact that GHC doesn't count an
identifier as ambiguous unless it is used to statically check that a
partial package is as "defined" (providing real implementations) as
possible.

## Test packages

A specification is not just type signatures: there are also semantic and
performance constraints on the implementations of these signatures.  The
test packages provide parametrized packages which, when instantiated
with an implementation of strings, tests if those constraints are
satisfied.

At the moment, there is only one test package: `str-tests-compare`,
which compares two implemenations of strings and ensure they behave
equivalently modulo `pack` and `unpack`.  In the future, we will
add other tests which will test properties we expect to hold
between implementations, laziness characteristics and perhaps
even performance.

Note that you will probably need to use the "partial" implementations
of various types to fulfill the signatures required by the test
packages.

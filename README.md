# backpack-str

This repository defines a collection of packages which
operate on strings (monomorphic sequences, if you like).
You can use these packages in two ways:

1. First, the packages `str-string`, `str-text`, `str-bytestring`
   and `str-foundation` provide *uniform*, monomorphic interfaces
   to all of the functionality offered by these packages.  You can
   transparently swap out one package for another, and as long as your
   code is using functions were are common to both packages, you can
   be assured the types of the identifiers do not change.  These
   packages smooth over a number of minor API differences between
   these packages.  These packages work on all versions of GHC.

2. Second, the `str-sig` package provides a Backpack signature (GHC 8.2
   only) that specifies the "full" abstract interface for strings
   (monomorphic sequences), abstracting over `Str` (the string type),
   `Chr` (the character type) and `Index` (the numeric type for indexing
   into strings; either `Int` or `Int64`); `str-sig` records the union
   of all functions supported by every package in this repository.

For more information, see the [README](str-sig/README.md) for str-sig.

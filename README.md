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

## Tutorial

Here's the short version:

1. Add `str-sig` to your `build-depends`.

2. Write your library, using functions from `Str` that were specified
   by `str-sig`.

3. Run [ghc-usage](https://hackage.haskell.org/package/ghc-usage)
   (`cabal new-repl -w ghc-usage`), and copy paste the export lists
   of the signatures into a `Str.hsig` file, so you end up with
   something like:

        signature Str( Str, Chr, Position, empty, append ) where
            import Prelude () -- don't omit this!

   Add the signature to your Cabal file with `signatures: Str`.
   If you add or remove functions you are using, modify the export
   list.

4. Your users can instantiate your library using one of the
   implementation packages.  For example, to use your library
   `foo-indef` with strings, a user writes `build-depends: foo-indef,
   str-string`.

Here's the long version.

First, go read [this blog post](http://blog.ezyang.com/2017/01/try-backpack-cabal-packages/),
which explains how to get and use Backpack on a simple example where
we write all the signatures by hand.  The rest of this tutorial will
assume a basic working knowledge on how to use Backpack.

The point of `str-sig` is to avoid having to write signatures by hand;
by simply adding `build-depends: str-sig`, we inherit all of the
requirements of `str-sig`.  There is one catch, however: `str-sig` is a
very *big* signature, which contains every function you could possibly
imagine on strings; in practice, an actual library is never going to use
all of these functions.  If we directly depend on `str-sig`, we are
claiming to depend on *everything* that `str-sig` defines, and that's a
problem, because none of the implementation packages actually defines
every function!

To solve this problem, after we include `str-sig`, we are going to
use *signature thinning* to reduce the set of entities which we
are going to require in the end.  A signature that thins its
exports looks like this:

    signature Str( Str, Chr, Position, empty, concat ) where
        import Prelude () -- don't omit this!

Here, the export list of the `Str` signature refers to declarations
which are not locally defined; in fact, they refer to the entities from
`Str` in `str-sig`.  You can think of this as a centralized import list:
just write all of the functions which you actually want to bring into
scope, and only those functions will be brought into scope when you
import `Str`.  The `import Prelude ()` ensures that an identifier like
`concat` doesn't get resolved to the `concat` from `Prelude`.

You can write this export list by hand, or you can generate
it automatically using [ghc-usage](https://hackage.haskell.org/package/ghc-usage);
just run `cabal new-repl -w ghc-usage --with-ghc-pkg=ghc-pkg-8.2` in
the library your running, and it will print out the export lists of
all your signatures (and modules too); copy paste them into `Str.hsig`.

## FAQ

**I'm not using GHC 8.2, is there anything to see here?**
The packages `str-string`, `str-text`, etc. are all compatible with
versions of GHC earlier than GHC 8.2.  If you prefer the uniformity
of these interfaces, you can use them directly (and it might make
it easier to go Backpack when you move to GHC 8.2.)

**How can I parametrize over Unicode strings specifically?**
In `Str.hsig`, add the line:

    type Chr = Char

This will set the type of characters to `Char`, letting you pass
characters into functions in the API, and requiring any implementation
of `Str` to also be Char-based.

Similarly, if you say:

    type Chr = Word8

You are now fixed to binary strings like `ByteString`.

**How do I instantiate a library with X?**
The general recipe is to add `mixins: str-IMPL (Str.IMPL as Str)` to
the Cabal file that wants to instantiate another one of its
dependencies.  Every package also has a "default" implementation
(strict, binary ByteStrings for `str-bytestring` and strict Text for
`str-text`) which can be used without a `mixin` field at all.

Here is some code you can copy-paste for each
string type you might want to use:

    -- String
    build-depends: lib-indef, str-string

    -- Strict Text (Data.Text)
    build-depends: lib-indef, str-text

    -- Lazy Text (Data.Text.Lazy)
    build-depends: lib-indef, str-text
    mixins: str-string (Str.Text.Lazy as Str)

    -- Strict Word8 ByteString (Data.ByteString)
    build-depends: lib-indef, str-bytestring

    -- Strict Char8 ByteString (Data.ByteString.Char8)
    build-depends: lib-indef, str-bytestring
    mixins: str-bytestring (Str.ByteString.Char8 as Str)

    -- Lazy Word8 ByteString (Data.ByteString)
    build-depends: lib-indef, str-bytestring
    mixins: str-bytestring (Str.ByteString.Lazy as Str)

    -- Lazy Char8 ByteString (Data.ByteString.Lazy.Char8)
    build-depends: lib-indef, str-bytestring
    mixins: str-bytestring (Str.ByteString.Lazy.Char8 as Str)

    -- Foundation (Foundation.String)
    build-depends: lib-indef, str-foundation

**The signature doesn't contain functions I need.**  Just add your
own functions to ``Str.hsig``!

    signature Str( Str, splits ) where
        import Prelude () -- don't omit this!
        data Str
        splits :: Str -> [(Str, Str)]

Note that you will need to declare `data Str`, etc., for any abstract
type you want to make reference to.

Obviously, the implementation packages we provide won't have
implementations of these functions, but you can easily define your
own implementation module (it will need to be in a library
or package separate from the one you're parametrizing over), reusing the
existing implementations:

    module Str.String.Splits (
        module Str.String,
        splits
    ) where
        import Str.String
        splits = ...

If you think your method is of general interest, submit a PR to
str-sig!

## Feature matrix

Although eventually we would like to have feature parity among
all implementations strings, at the moment, there are some gaps
in coverage in implementation of functions.  Below is a feature
matrix saying which functions are supported by which libraries.

Key | Module name
----|--------------------------
S   | Str.String
T   | Str.Text
TL  | Str.Text.Lazy
B   | Str.ByteString
BC  | Str.ByteString.Char8
BL  | Str.ByteString.Lazy
BLC | Str.ByteString.Lazy.Char8
F   | Str.Foundation

**String types**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
Str             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
Chr             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
Index           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔

**Introducing and eliminating strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
empty           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
singleton       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
pack            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
unpack          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔

**Basic interface**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
cons            | ✔ |   |   |   |   | ✔ | ✔ |  
cons'           |   | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
snoc            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
append          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
head            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
uncons          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
unsnoc          | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔
last            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
tail            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
init            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
null            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
length          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
compareLength   | ✔ | ✔ | ✔ |   |   |   |   |  

**Transforming strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
map             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
reverse         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
intersperse     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
intercalate     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
transpose       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
replace         |   | ✔ | ✔ |   |   |   |   |  

**Case conversion**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
toCaseFold      |   | ✔ | ✔ |   |   |   |   |  
toLower         |   | ✔ | ✔ |   |   |   |   |  
toUpper         |   | ✔ | ✔ |   |   |   |   |  
toTitle         |   | ✔ | ✔ |   |   |   |   |  

**Justification**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
justifyLeft     |   | ✔ | ✔ |   |   |   |   |  
justifyRight    |   | ✔ | ✔ |   |   |   |   |  
center          |   | ✔ | ✔ |   |   |   |   |  

**Reducing strings (folds)**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
foldl           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldl'          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldl1          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
foldl1'         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
foldr           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldr'          | ✔ |   |   | ✔ | ✔ |   |   | ✔
foldr1          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
foldr1'         |   |   |   | ✔ | ✔ |   |   |  

**Special folds**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
concat          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
concatMap       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
any             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
all             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
maximum         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
minimum         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔

**Building strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
scanl           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
scanl1          | ✔ | ✔ | ✔ | ✔ | ✔ |   |   |  
scanr           | ✔ | ✔ | ✔ | ✔ | ✔ |   |   |  
scanr1          | ✔ | ✔ | ✔ | ✔ | ✔ |   |   |  

**Accumulating maps**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
mapAccumL       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
mapAccumR       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  

**Infinite strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
repeat          | ✔ |   | ✔ |   |   | ✔ | ✔ |  
cycle           | ✔ |   | ✔ |   |   | ✔ | ✔ |  
iterate         | ✔ |   | ✔ |   |   | ✔ | ✔ |  

**Unfolds and replicates**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
replicate       | ✔ |   |   | ✔ | ✔ | ✔ | ✔ |  
concatReplicate | ✔ | ✔ | ✔ |   |   |   |   |  
unfoldr         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
unfoldrN        |   |   |   | ✔ | ✔ |   |   |  

**Substrings: Breaking strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
take            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
takeEnd         |   | ✔ | ✔ |   |   |   |   |  
drop            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
dropEnd         |   | ✔ | ✔ |   |   |   |   |  
splitAt         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
takeWhile       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
takeWhileEnd    |   | ✔ | ✔ |   |   |   |   |  
dropWhile       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
dropWhileEnd    |   | ✔ | ✔ |   |   |   |   |  
stripStart      |   | ✔ | ✔ |   |   |   |   |  
stripEnd        |   | ✔ | ✔ |   |   |   |   |  
strip           |   | ✔ | ✔ |   |   |   |   |  
span            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
spanEnd         |   |   |   | ✔ | ✔ |   |   |  
break           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
breakEnd        |   |   |   | ✔ | ✔ |   |   |  
breakOn         |   | ✔ | ✔ | ✔ | ✔ |   |   |  
breakOnEnd      |   | ✔ | ✔ |   |   |   |   |  
group           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
groupBy         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
inits           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
tails           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  

**Substrings: Breaking into many substrings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
splitOn         |   | ✔ | ✔ |   |   |   |   |  
splitWhen       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
chunksOf        |   | ✔ | ✔ |   |   |   |   |  

**Breaking into lines and words**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
lines           | ✔ | ✔ | ✔ |   | ✔ |   | ✔ | ✔
unlines         | ✔ | ✔ | ✔ |   | ✔ |   | ✔ |  
words           | ✔ | ✔ | ✔ |   | ✔ |   | ✔ | ✔
unwords         | ✔ | ✔ | ✔ |   | ✔ |   | ✔ |  

**Predicates**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
isPrefixOf      | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
isSuffixOf      | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
isInfixOf       | ✔ | ✔ | ✔ | ✔ | ✔ |   |   |  

**View patterns**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
stripPrefix     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
stripSuffix     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
commonPrefixes  |   | ✔ | ✔ |   |   |   |   |  

**Search for arbitrary substrings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
breakSubstring  |   |   |   | ✔ | ✔ |   |   |  
findSubstring   |   |   |   | ✔ | ✔ |   |   |  
findSubstrings  |   |   |   | ✔ | ✔ |   |   |  

**Searching by equality**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
elem            | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔
notElem         | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔

**Searching with a predicate**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
find            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
filter          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
partition       | ✔ | ✔ | ✔ | ✔ |   | ✔ |   | ✔
breakOnAll      |   | ✔ | ✔ |   |   |   |   |  

**Indexing strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
index           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
elemIndex       | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔
elemIndices     | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔
elemIndexEnd    |   |   |   | ✔ | ✔ | ✔ |   |  
elemCount       | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔
substringCount  |   | ✔ | ✔ |   |   |   |   |  
findIndex       | ✔ | ✔ |   | ✔ | ✔ | ✔ | ✔ | ✔
findIndices     | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔

**Zipping and unzipping**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
zip             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
zipWith         | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔
packZipWith     | ✔ | ✔ | ✔ |   |   |   |   |  
unzip           | ✔ |   |   | ✔ | ✔ | ✔ |   |  

**Ordered Strs**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
sort            | ✔ |   |   | ✔ | ✔ |   |   |  

**Copying strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
copy            | ✔ | ✔ |   | ✔ | ✔ | ✔ | ✔ |  

**Using as CString**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
packCString     | ✔ |   |   | ✔ | ✔ |   |   |  
packCStringLen  | ✔ |   |   | ✔ | ✔ |   |   |  

**Using as operating system string**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
useAsOSString   | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
newOSString     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
packOSString    | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔

**Reading integers from strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
readInt         |   |   |   |   | ✔ |   | ✔ |  
readInteger     |   |   |   |   | ✔ |   | ✔ |  

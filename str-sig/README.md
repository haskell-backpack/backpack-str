# str-sig

`str-sig` provides a Backpack signature (GHC 8.2 only) that specifies
the "full" abstract interface for strings (monomorphic sequences),
abstracting over `Str` (the string type), `Chr` (the character type) and
`Index` (the numeric type for indexing into strings; either `Int` or
`Int64`).

The packages `str-string`, `str-text`, `str-bytestring` and
`str-foundation` provide interfaces which are compatible with subsets of
`str-sig` (described in more detail below.)

## FAQ

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

**How can I help?** As you can tell in the feature matrix below, certain
operations on `Str` are not widely supported among the implementations.
Contributing tests and efficient implementations (the tests are
important) to upstream or this package would be greatly appreciated.

Note that some operations are not efficiently implementable, or not
implementable at all, on certain string representations, so full feature
parity is not achievable.

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

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | Str             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | Chr             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | Index           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |

**Introducing and eliminating strings**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | empty           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | singleton       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | pack            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | unpack          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |

**Basic interface**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | cons            | ✔ |   |   |   |   | ✔ | ✔ |   |
    | cons'           |   | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | snoc            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | append          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | head            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | uncons          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | unsnoc          | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔ |
    | last            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | tail            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | init            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | null            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | length          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | compareLength   | ✔ | ✔ | ✔ |   |   |   |   |   |

**Transforming strings**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | map             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | reverse         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | intersperse     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | intercalate     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | transpose       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |   |
    | replace         |   | ✔ | ✔ |   |   |   |   |   |

**Case conversion**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | toCaseFold      |   | ✔ | ✔ |   |   |   |   |   |
    | toLower         |   | ✔ | ✔ |   |   |   |   |   |
    | toUpper         |   | ✔ | ✔ |   |   |   |   |   |
    | toTitle         |   | ✔ | ✔ |   |   |   |   |   |

**Justification**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | justifyLeft     |   | ✔ | ✔ |   |   |   |   |   |
    | justifyRight    |   | ✔ | ✔ |   |   |   |   |   |
    | center          |   | ✔ | ✔ |   |   |   |   |   |

**Reducing strings (folds)**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | foldl           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | foldl'          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | foldl1          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |   |
    | foldl1'         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |   |
    | foldr           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | foldr'          | ✔ |   |   | ✔ | ✔ |   |   | ✔ |
    | foldr1          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |   |
    | foldr1'         |   |   |   | ✔ | ✔ |   |   |   |

**Special folds**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | concat          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | concatMap       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | any             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | all             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | maximum         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | minimum         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |

**Building strings**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | scanl           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |   |
    | scanl1          | ✔ | ✔ | ✔ | ✔ | ✔ |   |   |   |
    | scanr           | ✔ | ✔ | ✔ | ✔ | ✔ |   |   |   |
    | scanr1          | ✔ | ✔ | ✔ | ✔ | ✔ |   |   |   |

**Accumulating maps**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | mapAccumL       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |   |
    | mapAccumR       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |   |

**Infinite strings**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | repeat          | ✔ |   | ✔ |   |   | ✔ | ✔ |   |
    | cycle           | ✔ |   | ✔ |   |   | ✔ | ✔ |   |
    | iterate         | ✔ |   | ✔ |   |   | ✔ | ✔ |   |

**Unfolds and replicates**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | replicate       | ✔ |   |   | ✔ | ✔ | ✔ | ✔ |   |
    | concatReplicate | ✔ | ✔ | ✔ |   |   |   |   |   |
    | unfoldr         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |   |
    | unfoldrN        |   |   |   | ✔ | ✔ |   |   |   |

**Substrings: Breaking strings**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | take            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | takeEnd         |   | ✔ | ✔ |   |   |   |   |   |
    | drop            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | dropEnd         |   | ✔ | ✔ |   |   |   |   |   |
    | splitAt         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | takeWhile       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | takeWhileEnd    |   | ✔ | ✔ |   |   |   |   |   |
    | dropWhile       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | dropWhileEnd    |   | ✔ | ✔ |   |   |   |   |   |
    | stripStart      |   | ✔ | ✔ |   |   |   |   |   |
    | stripEnd        |   | ✔ | ✔ |   |   |   |   |   |
    | strip           |   | ✔ | ✔ |   |   |   |   |   |
    | span            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | spanEnd         |   |   |   | ✔ | ✔ |   |   |   |
    | break           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | breakEnd        |   |   |   | ✔ | ✔ |   |   |   |
    | breakOn         |   | ✔ | ✔ | ✔ | ✔ |   |   |   |
    | breakOnEnd      |   | ✔ | ✔ |   |   |   |   |   |
    | group           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |   |
    | groupBy         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |   |
    | inits           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |   |
    | tails           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |   |

**Substrings: Breaking into many substrings**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | splitOn         |   | ✔ | ✔ |   |   |   |   |   |
    | splitWhen       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | chunksOf        |   | ✔ | ✔ |   |   |   |   |   |

**Breaking into lines and words**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | lines           | ✔ | ✔ | ✔ |   | ✔ |   | ✔ | ✔ |
    | unlines         | ✔ | ✔ | ✔ |   | ✔ |   | ✔ |   |
    | words           | ✔ | ✔ | ✔ |   | ✔ |   | ✔ | ✔ |
    | unwords         | ✔ | ✔ | ✔ |   | ✔ |   | ✔ |   |

**Predicates**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | isPrefixOf      | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | isSuffixOf      | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | isInfixOf       | ✔ | ✔ | ✔ | ✔ | ✔ |   |   |   |

**View patterns**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | stripPrefix     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | stripSuffix     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | commonPrefixes  |   | ✔ | ✔ |   |   |   |   |   |

**Search for arbitrary substrings**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | breakSubstring  |   |   |   | ✔ | ✔ |   |   |   |
    | findSubstring   |   |   |   | ✔ | ✔ |   |   |   |
    | findSubstrings  |   |   |   | ✔ | ✔ |   |   |   |

**Searching by equality**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | elem            | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔ |
    | notElem         | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔ |

**Searching with a predicate**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | find            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | filter          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | partition       | ✔ | ✔ | ✔ | ✔ |   | ✔ |   | ✔ |
    | breakOnAll      |   | ✔ | ✔ |   |   |   |   |   |

**Indexing strings**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | index           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | elemIndex       | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔ |
    | elemIndices     | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔ |
    | elemIndexEnd    |   |   |   | ✔ | ✔ | ✔ |   |   |
    | elemCount       | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔ |
    | substringCount  |   | ✔ | ✔ |   |   |   |   |   |
    | findIndex       | ✔ | ✔ |   | ✔ | ✔ | ✔ | ✔ | ✔ |
    | findIndices     | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔ |

**Zipping and unzipping**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | zip             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | zipWith         | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔ |
    | packZipWith     | ✔ | ✔ | ✔ |   |   |   |   |   |
    | unzip           | ✔ |   |   | ✔ | ✔ | ✔ |   |   |

**Ordered strings**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | sort            | ✔ |   |   | ✔ | ✔ |   |   |   |

**Copying strings**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | copy            | ✔ | ✔ |   | ✔ | ✔ | ✔ | ✔ |   |

**Using as CString**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | packCString     | ✔ |   |   | ✔ | ✔ |   |   |   |
    | packCStringLen  | ✔ |   |   | ✔ | ✔ |   |   |   |

**Using as operating system string**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | useAsOSString   | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | newOSString     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |
    | packOSString    | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |

**Reading integers from strings**

    |                 | S | T | TL| B | BC| BL|BLC| F |
    |-----------------|---|---|---|---|---|---|---|---|
    | readInt         |   |   |   |   | ✔ |   | ✔ |   |
    | readInteger     |   |   |   |   | ✔ |   | ✔ |   |

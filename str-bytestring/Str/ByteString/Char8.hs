{-# LANGUAGE Trustworthy #-}
-- |
-- Module      : Str.ByteString.Char8
-- Copyright   : (c) Edward Z. Yang 2017
-- License     : BSD-style
-- Maintainer  : ezyang@mit.edu
-- Stability   : unstable
-- Portability : non-portable
--
-- An adaptor module for "Data.ByteString.Char8" which fulls the Str
-- signature from the str-sig package.  The implementation of strings
-- this module implements is a time and space-efficient implementation
-- of byte vectors using packed Word8 arrays.  This module provides
-- an API which interprets the each word of the string as an 8-b9t
-- character.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.
--
-- > import qualified Str.ByteString.Char8 as S
--
-- Rather than import this module directly, consider parametrizing your
-- package using str-sig instead!
--
module Str.ByteString.Char8 (
    -- * String types
    Str,
    Chr,
    Index,

    -- * Introducing and eliminating 'Str'
    empty,
    singleton,
    pack,
    unpack,

    -- * Basic interface
    -- cons,
    cons',
    snoc,
    append,
    head,
    uncons,
    unsnoc,
    last,
    tail,
    init,
    null,
    length,
    -- compareLength,

    -- * Transforming strings
    map,
    reverse,
    intersperse,
    intercalate,
    transpose,
    -- replace,

    -- * Case conversion
    -- toCaseFold,
    -- toLower,
    -- toUpper,
    -- toTitle,

    -- * Justification
    -- justifyLeft,
    -- justifyRight,
    -- center,

    -- * Reducing strings (folds)
    foldl,
    foldl',
    foldl1,
    foldl1',
    foldr,
    foldr',
    foldr1,
    foldr1',

    -- * Special folds
    concat,
    concatMap,
    any,
    all,
    maximum,
    minimum,

    -- * Building 'Str's
    scanl,
    scanl1,
    scanr,
    scanr1,

    -- * Accumulating maps
    mapAccumL,
    mapAccumR,

    -- * Infinite strings
    -- repeat,
    -- cycle,
    -- iterate,

    -- * Unfolds and replicates
    replicate,
    -- concatReplicate,
    unfoldr,
    unfoldrN,

    -- * Substrings: Breaking strings
    take,
    -- takeEnd,
    drop,
    -- dropEnd,
    splitAt,
    takeWhile,
    -- takeWhileEnd,
    dropWhile,
    -- dropWhileEnd,
    -- stripStart,
    -- stripEnd,
    -- strip,
    span,
    spanEnd,
    break,
    breakEnd,
    breakOn,
    -- breakOnEnd,
    group,
    groupBy,
    inits,
    tails,
    stripPrefix,
    stripSuffix,

    -- * Substrings: Breaking into many substrings
    -- splitOn,
    -- splitWhen,
    -- chunksOf,

    -- * Breaking into lines and words
    lines,
    unlines,
    words,
    unwords,

    -- * Predicates
    isPrefixOf,
    isSuffixOf,
    isInfixOf,

    -- * View patterns
    -- commonPrefixes,

    -- * Search for arbitrary substrings
    breakSubstring,
    findSubstring,
    findSubstrings,

    -- * Searching by equality
    elem,
    notElem,

    -- * Searching with a predicate
    find,
    filter,
    -- partition,
    -- breakOnAll,

    -- * Indexing 'Str's
    index,
    elemIndex,
    elemIndices,
    elemIndexEnd,
    elemCount,
    -- substringCount,
    findIndex,
    findIndices,

    -- * Zipping and unzipping
    zip,
    zipWith,
    -- packZipWith,
    unzip,

    -- * Ordered Strs
    sort,

    -- * Copying Strs
    copy,

    -- * Using Str as CString
    packCString,
    packCStringLen,

    -- * Using Str as operating system string
    useAsOSString,
    newOSString,
    packOSString,

    -- * Reading from Str
    readInt,
    readInteger,
) where

import Prelude (Int, IO, Monad(..), Bool, Num(..), ($), Char)
import Foreign.C.String (CString)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Data.ByteString.Char8
import Data.ByteString.Unsafe
import Data.Word (Word8)

-- | A space-efficient representation of a 'Word8' vector, supporting many
-- efficient operations.  Corresponds to 'ByteString' from the
-- bytestring library.
--
type Str = ByteString

-- | The characters of a 'ByteString' are characters, truncated at 8
-- bits.
--
type Chr = Char

-- | The length and positions of characters within a 'Str' are
-- measured with machine-precision 'Int'.
--
type Index = Int

-- | /O(n) construction/ Use a 'Str' with a function requiring a
-- null-terminated @CString@.  The @CString@ is a copy and will be freed
-- automatically; it must not be stored or used after the
-- subcomputation finishes.  Corresponds to 'useAsCString' from
-- the bytestring library.
--
useAsOSString :: Str -> (CString -> IO a) -> IO a
useAsOSString = useAsCString

-- | /O(n)/.  Marshal a 'Str' into a NUL terminated C string.
--
-- * New storage is allocated for the C string and must be
--   explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
newOSString   :: Str -> IO CString
newOSString s = unsafeUseAsCStringLen s $ \(c,l) -> do
                    p <- mallocBytes (l+1)
                    copyBytes p c l
                    pokeByteOff p l (0::Word8)
                    return p

-- | /O(n)./ Construct a new @ByteString@ from a @CString@. The
-- resulting @ByteString@ is an immutable copy of the original
-- @CString@, and is managed on the Haskell heap. The original
-- @CString@ must be null terminated.  Corresponds to 'packCString'
-- from the bytestring library.
--
packOSString  :: CString -> IO Str
packOSString  = packCString

-- | Break a string on a substring, returning a pair of the part of the
-- string prior to the match, and the rest of the string.  Corresponds
-- to 'breakSubstring' from the bytestring library.
--
breakOn :: Str -> Str -> (Str, Str)
breakOn = breakSubstring

-- bytestring's implementation is buggy
--  splitWhen :: (Chr -> Bool) -> Str -> [Str]
--  splitWhen = splitWith

-- | /O(n)/ elemCount returns the number of times its argument appears in the Str
--
-- > elemCount = length . elemIndices
--
-- Corresponds to 'count' from the bytestring library.
--
elemCount = count

-- | /O(n)/ Analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.  Corresponds to
-- 'cons' from the bytestring library.
--
cons' :: Chr -> Str -> Str
cons' = cons

infixr 5 `cons'`

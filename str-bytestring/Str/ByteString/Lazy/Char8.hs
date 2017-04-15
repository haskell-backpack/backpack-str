{-# LANGUAGE Trustworthy #-}
-- |
-- Module      : Str.ByteString.Lazy.Char8
-- Copyright   : (c) Edward Z. Yang 2017
-- License     : BSD-style
-- Maintainer  : ezyang@mit.edu
-- Stability   : unstable
-- Portability : non-portable
--
-- An adaptor module for "Data.ByteString.Lazy.Char8" which fulls the Str
-- signature from the str-sig package.  The implementation of strings
-- this module implements is a lazy byte vectors, encoded as lazy
-- lists of strict chunks of bytes.  This module provides
-- an API which interprets the characters of these strings as characters
-- truncated to 8-bits.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.
--
-- > import qualified Str.ByteString.Lazy.Char8 as S
--
-- Rather than import this module directly, consider parametrizing your
-- package using str-sig instead!
--
module Str.ByteString.Lazy.Char8 (
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
    cons,
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
    -- foldr',
    foldr1,
    -- foldr1',

    -- * Special folds
    concat,
    concatMap,
    any,
    all,
    maximum,
    minimum,

    -- * Building 'Str's
    scanl,
    -- scanl1,
    -- scanr,
    -- scanr1,

    -- * Accumulating maps
    mapAccumL,
    mapAccumR,

    -- * Infinite strings
    repeat,
    cycle,
    iterate,

    -- * Unfolds and replicates
    replicate,
    -- concatReplicate,
    unfoldr,
    -- unfoldrN,

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
    -- spanEnd,
    break,
    -- breakEnd,
    -- breakOn,
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
    -- isInfixOf,

    -- * View patterns
    -- commonPrefixes,

    -- * Search for arbitrary substrings
    -- breakSubstring,
    -- findSubstring,
    -- findSubstrings,

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
    -- elemIndexEnd,
    elemCount,
    -- substringCount,
    findIndex,
    findIndices,

    -- * Zipping and unzipping
    zip,
    zipWith,
    -- packZipWith,
    -- unzip,

    -- * Ordered Strs
    -- sort,

    -- * Copying Strs
    copy,

    -- * Using Str as CString
    -- packCString,
    -- packCStringLen,

    -- * Using Str as operating system string
    useAsOSString,
    newOSString,
    packOSString,

    -- * Reading from Str
    readInt,
    readInteger,
) where

import Prelude (IO, Bool, (.), Functor(..), Char)
import Foreign.C
import Data.Int
import Data.Word
import Data.ByteString.Lazy.Char8
import qualified Str.ByteString as S

-- | A space-efficient representation of a lazy 'Word8' vector,
-- supporting many efficient operations.  Corresponds to lazy
-- 'ByteString' from the bytestring library.
--
type Str = ByteString

-- | The characters of a 'ByteString' are 8-bit truncated characters.
--
type Chr = Char

-- | The length and positions of characters within a 'Str' are
-- measured with 64-bit integers; this avoids overflow when
-- streaming over more than 4G of data on architectures with
-- native 32-bit integers.
--
type Index = Int64

-- | /O(n) construction/ Use a 'Str' with a function requiring a
-- null-terminated @CString@.  The @CString@ is a copy and will be freed
-- automatically; it must not be stored or used after the
-- subcomputation finishes.
--
-- This function will force the entirety of a lazy 'ByteString'.
--
useAsOSString :: Str -> (CString -> IO a) -> IO a
useAsOSString s f = S.useAsOSString (toStrict s) f

-- | /O(n)/.  Marshal a 'Str' into a NUL terminated C string.
--
-- * New storage is allocated for the C string and must be
--   explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
-- This function will force the entirety of a lazy 'ByteString'.
--
newOSString   :: Str -> IO CString
newOSString s = S.newOSString (toStrict s)

-- | /O(n)./ Construct a new @ByteString@ from a @CString@. The
-- resulting @ByteString@ is an immutable copy of the original
-- @CString@, and is managed on the Haskell heap. The original
-- @CString@ must be null terminated.
--
-- This function will force the entirety of a lazy 'ByteString'.
--
packOSString  :: CString -> IO Str
packOSString = fmap fromStrict . S.packOSString

-- bytestring is buggy
-- splitWhen :: (Chr -> Bool) -> Str -> [Str]
-- splitWhen = splitWith

-- | /O(n)/ elemCount returns the number of times its argument appears in the Str
--
-- > elemCount = length . elemIndices
--
-- Corresponds to 'count' from the bytestring library.
--
elemCount :: Chr -> Str -> Index
elemCount = count

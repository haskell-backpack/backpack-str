{-# LANGUAGE Trustworthy #-}
-- |
-- Module      : Str.Text.Lazy
-- Copyright   : (c) Edward Z. Yang 2017
-- License     : BSD-style
-- Maintainer  : ezyang@mit.edu
-- Stability   : unstable
-- Portability : non-portable
--
-- An adaptor module for "Data.Text.Lazy" which fulls the Str
-- signature from the str-sig package.  These strings are internally
-- implemented as lazily linked lists of packed UTF-16 data.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.
--
-- > import qualified Str.Text.Lazy as S
--
-- Rather than import this module directly, consider parametrizing your
-- package using str-sig instead!
--
module Str.Text.Lazy (
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
    -- unsnoc,
    last,
    tail,
    init,
    null,
    length,
    compareLength,

    -- * Transforming strings
    map,
    reverse,
    intersperse,
    intercalate,
    transpose,
    replace,

    -- * Case conversion
    toCaseFold,
    toLower,
    toUpper,
    toTitle,

    -- * Justification
    justifyLeft,
    justifyRight,
    center,

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
    scanl1,
    scanr,
    scanr1,

    -- * Accumulating maps
    mapAccumL,
    mapAccumR,

    -- * Infinite strings
    repeat,
    cycle,
    iterate,

    -- * Unfolds and replicates
    -- replicate,
    concatReplicate,
    unfoldr,
    -- unfoldrN,

    -- * Substrings: Breaking strings
    take,
    takeEnd,
    drop,
    dropEnd,
    splitAt,
    takeWhile,
    takeWhileEnd,
    dropWhile,
    dropWhileEnd,
    stripStart,
    stripEnd,
    strip,
    span,
    -- spanEnd,
    break,
    -- breakEnd,
    breakOn,
    breakOnEnd,
    group,
    groupBy,
    inits,
    tails,
    stripPrefix,
    stripSuffix,

    -- * Substrings: Breaking into many substrings
    splitOn,
    splitWhen,
    chunksOf,

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
    commonPrefixes,

    -- * Search for arbitrary substrings
    -- breakSubstring,
    -- findSubstring,
    -- findSubstrings,

    -- * Searching by equality
    -- elem,
    -- notElem,

    -- * Searching with a predicate
    find,
    filter,
    partition,
    breakOnAll,

    -- * Indexing 'Str's
    index,
    -- elemIndex,
    -- elemIndices,
    -- elemIndexEnd,
    -- elemCount,
    substringCount,
    -- findIndex,
    -- findIndices,

    -- * Zipping and unzipping
    zip,
    -- zipWith,
    packZipWith,
    -- unzip,

    -- * Ordered Strs
    -- sort,

    -- * Copying Strs
    -- copy,

    -- * Using Str as CString
    -- packCString,
    -- packCStringLen,

    -- * Using Str as operating system string
    useAsOSString,
    newOSString,
    packOSString,

    -- * Reading from Str
    -- readInt,
    -- readInteger,
) where

import Prelude (Char, Int, Bool, IO, fmap)
import System.Posix.Internals (CFilePath, newFilePath, withFilePath, peekFilePath)
import Data.Int (Int64)

import Data.Text.Lazy hiding (snoc)
import qualified Data.Text.Lazy as TL

-- | A lazily linked list of space-efficient, packed UTF-16 encoded
-- Unicode string, supporting many efficient operations.  Corresponds to
-- lazy 'Text' from the text library.
--
type Str = Text

-- | The characters of a 'Text' are Unicode scalar values.
--
type Chr = Char

-- | The length and positions of characters within a 'Str' are
-- measured with 64-bit integers; this avoids overflow when
-- streaming over more than 4G of data on architectures with
-- native 32-bit integers.
--
type Index = Int64

-- | /O(n)/ Analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.  Corresponds to
-- 'cons' from the text library.
--
cons' :: Chr -> Str -> Str
cons' = cons

infixr 5 `cons'`

-- | /O(n)/ Append a character onto the end of a 'Str'.
--
snoc :: Str -> Chr -> Str
snoc = TL.snoc

infixl 5 `snoc`

-- | /O(n)/ Splits a 'Str' into components delimited by
-- separators, where the predicate returns True for a separator character.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.
--
-- This is called 'split' in the text library.
--
splitWhen :: (Chr -> Bool) -> Str -> [Str]
splitWhen = split

-- | /O(n+m)/ The 'substringCount' function returns the number of times the
-- query string appears in the given 'Str'. An empty query string is
-- invalid, and will cause an error to be raised.
--
-- This function is referred to as 'count' in the text library.
--
substringCount :: Str -> Str -> Index
substringCount = count

-- | /O(n)/ 'packZipWith' is like 'zipWith', but packs the result
-- directly into a 'Str'.
--
-- This function is called 'Data.Text.Lazy.zipWith' in in the text library.
--
packZipWith :: (Char -> Char -> Char) -> Str -> Str -> Str
packZipWith = zipWith

-- | /O(n)/ 'concatReplicate' @n x@ is a 'Str' consisting of @x@
-- repeated @n@ times.  This is called @replicate@ in the text library.
--
concatReplicate :: Index -> Str -> Str
concatReplicate = replicate

-- TODO: Make these more efficient

-- | Marshal a 'Str' into a NUL terminated, PEP 383 encoded, C string
-- using temporary storage.  (NB: in general, this won't be UTF-16
-- encoded.)
--
useAsOSString :: Str -> (CFilePath -> IO a) -> IO a
useAsOSString s f = withFilePath (unpack s) f

-- | Marshal a 'Str' into a NUL terminated, PEP 383 encoded, C string.
-- (NB: in general, this won't be UTF-16 encoded.)
--
newOSString   :: Str -> IO CFilePath
newOSString s = newFilePath (unpack s)

-- | Marshal a NUL terminated, PEP 383 encoded, C string into a 'Str'.
--
packOSString  :: CFilePath -> IO Str
packOSString c = fmap pack (peekFilePath c)

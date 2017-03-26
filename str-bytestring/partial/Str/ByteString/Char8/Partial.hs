module Str.ByteString.Char8.Partial (
    -- * String types
    Str,
    Chr,
    Index,

    -- * Introducing and eliminating strings
    S.empty,
    S.singleton,
    S.pack,
    S.unpack,

    -- * Basic interface
    cons,
    S.cons',
    S.snoc,
    S.append,
    S.head,
    S.uncons,
    S.unsnoc,
    S.last,
    S.tail,
    S.init,
    S.null,
    S.length,
    compareLength,

    -- * Transforming strings
    S.map,
    S.reverse,
    S.intersperse,
    S.intercalate,
    S.transpose,
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
    S.foldl,
    S.foldl',
    S.foldl1,
    S.foldl1',
    S.foldr,
    S.foldr',
    S.foldr1,
    S.foldr1',

    -- * Special folds
    S.concat,
    S.concatMap,
    S.any,
    S.all,
    S.maximum,
    S.minimum,

    -- * Building strings
    S.scanl,
    S.scanl1,
    S.scanr,
    S.scanr1,

    -- * Accumulating maps
    S.mapAccumL,
    S.mapAccumR,

    -- * Infinite strings
    repeat,
    cycle,
    iterate,

    -- * Unfolds and replicates
    S.replicate,
    concatReplicate,
    S.unfoldr,
    S.unfoldrN,

    -- * Substrings: Breaking strings
    S.take,
    takeEnd,
    S.drop,
    dropEnd,
    S.splitAt,
    S.takeWhile,
    takeWhileEnd,
    S.dropWhile,
    dropWhileEnd,
    stripStart,
    stripEnd,
    strip,
    S.span,
    S.spanEnd,
    S.break,
    S.breakEnd,
    S.breakOn,
    breakOnEnd,
    S.group,
    S.groupBy,
    S.inits,
    S.tails,

    -- * Substrings: Breaking into many substrings
    splitOn,
    splitWhen,
    chunksOf,

    -- * Breaking into lines and words
    S.lines,
    S.unlines,
    S.words,
    S.unwords,

    -- * Predicates
    S.isPrefixOf,
    S.isSuffixOf,
    S.isInfixOf,

    -- * View patterns
    S.stripPrefix,
    S.stripSuffix,
    commonPrefixes,

    -- * Search for arbitrary substrings
    S.breakSubstring,
    S.findSubstring,
    S.findSubstrings,

    -- * Searching by equality
    S.elem,
    S.notElem,

    -- * Searching with a predicate
    S.find,
    S.filter,
    partition,
    breakOnAll,

    -- * Indexing strings
    S.index,
    S.elemIndex,
    S.elemIndices,
    S.elemIndexEnd,
    S.elemCount,
    substringCount,
    S.findIndex,
    S.findIndices,

    -- * Zipping and unzipping
    S.zip,
    S.zipWith,
    packZipWith,
    S.unzip,

    -- * Ordered strings
    S.sort,

    -- * Copying strings
    S.copy,

    -- * Using as CString
    S.packCString,
    S.packCStringLen,

    -- * Using as operating system string
    S.useAsOSString,
    S.newOSString,
    S.packOSString,

    -- * Reading integers from strings
    S.readInt,
    S.readInteger,
) where

import Prelude ()
import Str.ByteString.Char8
import qualified Str.ByteString.Char8 as S
import Str.ByteString.Char8.Undefined

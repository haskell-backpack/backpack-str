module Str.String.Partial (
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
    S.cons,
    cons',
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
    S.compareLength,

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
    foldr1',

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
    S.repeat,
    S.cycle,
    S.iterate,

    -- * Unfolds and replicates
    S.replicate,
    S.concatReplicate,
    S.unfoldr,
    unfoldrN,

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
    spanEnd,
    S.break,
    breakEnd,
    breakOn,
    breakOnEnd,
    S.group,
    S.groupBy,
    S.inits,
    S.tails,

    -- * Substrings: Breaking into many substrings
    splitOn,
    S.splitWhen,
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
    breakSubstring,
    findSubstring,
    findSubstrings,

    -- * Searching by equality
    S.elem,
    S.notElem,

    -- * Searching with a predicate
    S.find,
    S.filter,
    S.partition,
    breakOnAll,

    -- * Indexing strings
    S.index,
    S.elemIndex,
    S.elemIndices,
    elemIndexEnd,
    S.elemCount,
    substringCount,
    S.findIndex,
    S.findIndices,

    -- * Zipping and unzipping
    S.zip,
    S.zipWith,
    S.packZipWith,
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
    readInt,
    readInteger,
) where

import Prelude ()
import Str.String
import qualified Str.String as S
import Str.String.Undefined

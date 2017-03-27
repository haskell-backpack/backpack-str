module Str.Foundation.Partial (
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
    S.foldl,
    S.foldl',
    foldl1,
    foldl1',
    S.foldr,
    S.foldr',
    foldr1,
    foldr1',

    -- * Special folds
    S.concat,
    S.concatMap,
    S.any,
    S.all,
    S.maximum,
    S.minimum,

    -- * Building strings
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
    replicate,
    concatReplicate,
    unfoldr,
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
    group,
    groupBy,
    inits,
    tails,

    -- * Substrings: Breaking into many substrings
    splitOn,
    S.splitWhen,
    chunksOf,

    -- * Breaking into lines and words
    S.lines,
    unlines,
    S.words,
    unwords,

    -- * Predicates
    S.isPrefixOf,
    S.isSuffixOf,
    isInfixOf,

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
    packZipWith,
    unzip,

    -- * Ordered strings
    sort,

    -- * Copying strings
    copy,

    -- * Using as CString
    packCString,
    packCStringLen,

    -- * Using as operating system string
    S.useAsOSString,
    S.newOSString,
    S.packOSString,

    -- * Reading integers from strings
    readInt,
    readInteger,
) where

import Prelude ()
import Str.Foundation
import qualified Str.Foundation as S
import Str.Foundation.Undefined

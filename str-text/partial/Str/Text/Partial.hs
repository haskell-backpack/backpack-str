module Str.Text.Partial (
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
    unsnoc,
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
    S.replace,

    -- * Case conversion
    S.toCaseFold,
    S.toLower,
    S.toUpper,
    S.toTitle,

    -- * Justification
    S.justifyLeft,
    S.justifyRight,
    S.center,

    -- * Reducing strings (folds)
    S.foldl,
    S.foldl',
    S.foldl1,
    S.foldl1',
    S.foldr,
    foldr',
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
    repeat,
    cycle,
    iterate,

    -- * Unfolds and replicates
    replicate,
    S.concatReplicate,
    S.unfoldr,
    unfoldrN,

    -- * Substrings: Breaking strings
    S.take,
    S.takeEnd,
    S.drop,
    S.dropEnd,
    S.splitAt,
    S.takeWhile,
    S.takeWhileEnd,
    S.dropWhile,
    S.dropWhileEnd,
    S.stripStart,
    S.stripEnd,
    S.strip,
    S.span,
    spanEnd,
    S.break,
    breakEnd,
    S.breakOn,
    S.breakOnEnd,
    S.group,
    S.groupBy,
    S.inits,
    S.tails,

    -- * Substrings: Breaking into many substrings
    S.splitOn,
    S.splitWhen,
    S.chunksOf,

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
    S.commonPrefixes,

    -- * Search for arbitrary substrings
    breakSubstring,
    findSubstring,
    findSubstrings,

    -- * Searching by equality
    elem,
    notElem,

    -- * Searching with a predicate
    S.find,
    S.filter,
    S.partition,
    S.breakOnAll,

    -- * Indexing strings
    S.index,
    elemIndex,
    elemIndices,
    elemIndexEnd,
    elemCount,
    S.substringCount,
    S.findIndex,
    findIndices,

    -- * Zipping and unzipping
    S.zip,
    zipWith,
    S.packZipWith,
    unzip,

    -- * Ordered strings
    sort,

    -- * Copying strings
    S.copy,

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
import Str.Text
import qualified Str.Text as S
import Str.Text.Undefined

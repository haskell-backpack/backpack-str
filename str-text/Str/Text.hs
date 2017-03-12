module Str.Text (
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
    findIndex,
    -- findIndices,

    -- * Zipping and unzipping
    zip,
    -- zipWith,
    packZipWith,
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
    -- readInt,
    -- readInteger,
) where

import Prelude (Char, Int, Bool, IO, fmap)
import System.Posix.Internals (CFilePath, newFilePath, withFilePath, peekFilePath)

import Data.Text

type Str = Text
type Chr = Char
type Index = Int

cons' :: Chr -> Str -> Str
cons' = cons

infixr 5 `cons'`

splitWhen :: (Chr -> Bool) -> Str -> [Str]
splitWhen = split

substringCount :: Str -> Str -> Index
substringCount = count

packZipWith :: (Char -> Char -> Char) -> Str -> Str -> Str
packZipWith = zipWith

concatReplicate :: Index -> Str -> Str
concatReplicate = replicate

-- unfoldrN doesn't return the seed, so it's no good

-- TODO: Make these more efficient

useAsOSString :: Str -> (CFilePath -> IO a) -> IO a
useAsOSString s f = withFilePath (unpack s) f

newOSString   :: Str -> IO CFilePath
newOSString s = newFilePath (unpack s)

packOSString  :: CFilePath -> IO Str
packOSString c = fmap pack (peekFilePath c)

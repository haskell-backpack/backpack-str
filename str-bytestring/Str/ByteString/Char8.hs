{-# LANGUAGE Trustworthy #-}
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
    splitWhen,
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

type Str = ByteString
type Chr = Char
type Index = Int

useAsOSString :: Str -> (CString -> IO a) -> IO a
useAsOSString = useAsCString

newOSString   :: Str -> IO CString
newOSString s = unsafeUseAsCStringLen s $ \(c,l) -> do
                    p <- mallocBytes (l+1)
                    copyBytes p c l
                    pokeByteOff p l (0::Word8)
                    return p

packOSString  :: CString -> IO Str
packOSString  = packCString

breakOn :: Str -> Str -> (Str, Str)
breakOn = breakSubstring

splitWhen :: (Chr -> Bool) -> Str -> [Str]
splitWhen = splitWith

-- | /O(n)/ elemCount returns the number of times its argument appears in the Str
--
-- > elemCount = length . elemIndices
--
elemCount = count

cons' :: Chr -> Str -> Str
cons' = cons

infixr 5 `cons'`

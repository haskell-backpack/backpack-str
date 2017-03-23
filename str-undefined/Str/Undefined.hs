-- | Trivial implementation of str-sig where all functions are
-- undefined.  This is useful sometimes.
module Str.Undefined (
    -- * String types
    Str,
    Chr,
    Index,

    -- * Introducing and eliminating strings
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
    spanEnd,
    break,
    breakEnd,
    breakOn,
    breakOnEnd,
    group,
    groupBy,
    inits,
    tails,

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
    stripPrefix,
    stripSuffix,
    commonPrefixes,

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
    partition,
    breakOnAll,

    -- * Indexing strings
    index,
    elemIndex,
    elemIndices,
    elemIndexEnd,
    elemCount,
    substringCount,
    findIndex,
    findIndices,

    -- * Zipping and unzipping
    zip,
    zipWith,
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
    useAsOSString,
    newOSString,
    packOSString,

    -- * Reading integers from strings
    readInt,
    readInteger,
) where

import Prelude (Show, Real, Read, Num, Integral, Enum, Ord, Eq, Monoid,
                Integer, Maybe, Int, IO, Bool, Ordering, error, String, (++))

import System.IO        (Handle)
import Foreign.C        (CString, CStringLen)
import Data.String      (IsString)
import Data.Data        (Data)
import Data.Ix          (Ix)

import Str

u :: String -> a
u s = error ("Str.Undefined: " ++ s)

-- --------------------------------------------------------------------
-- Introducing and eliminating 'Str'
--

empty :: Str
empty = u "empty"

singleton :: Chr -> Str
singleton _ = u "singleton"

pack :: [Chr] -> Str
pack _ = u "pack"

unpack :: Str -> [Chr]
unpack _ = u "unpack"

-- --------------------------------------------------------------------
-- Basic interface
--

cons :: Chr -> Str -> Str
cons _ _ = u "cons"

infixr 5 `cons`

cons' :: Chr -> Str -> Str
cons' _ _ = u "cons'"

infixr 5 `cons'`

snoc :: Str -> Chr -> Str
snoc _ _ = u "snoc"

infixl 5 `snoc`

append :: Str -> Str -> Str
append _ _ = u "append"

head :: Str -> Chr
head _ = u "head"

uncons :: Str -> Maybe (Chr, Str)
uncons _ = u "uncons"

unsnoc :: Str -> Maybe (Str, Chr)
unsnoc _ = u "unsnoc"

last :: Str -> Chr
last _ = u "last"

tail :: Str -> Str
tail _ = u "tail"

init :: Str -> Str
init _ = u "init"

null :: Str -> Bool
null _ = u "null"

length :: Str -> Index
length _ = u "length"

compareLength :: Str -> Index -> Ordering
compareLength _ _ = u "compareLength"

-- --------------------------------------------------------------------
-- Transforming strings
--

map :: (Chr -> Chr) -> Str -> Str
map _ _ = u "map"

reverse :: Str -> Str
reverse _ = u "reverse"

intersperse :: Chr -> Str -> Str
intersperse _ _ = u "intersperse"

intercalate :: Str -> [Str] -> Str
intercalate _ _ = u "intercalate"

transpose :: [Str] -> [Str]
transpose _ = u "transpose"

replace :: Str -> Str -> Str -> Str
replace _ _ _ = u "replace"

-- --------------------------------------------------------------------
-- Case conversion
--

toCaseFold :: Str -> Str
toCaseFold _ = u "toCaseFold"

toLower :: Str -> Str
toLower _ = u "toLower"

toUpper :: Str -> Str
toUpper _ = u "toUpper"

toTitle :: Str -> Str
toTitle _ = u "toTitle"

-- --------------------------------------------------------------------
-- Justification
--

justifyLeft :: Index -> Chr -> Str -> Str
justifyLeft _ _ _ = u "justifyLeft"

justifyRight :: Index -> Chr -> Str -> Str
justifyRight _ _ _ = u "justifyRight"

center :: Index -> Chr -> Str -> Str
center _ _ _ = u "center"

-- --------------------------------------------------------------------
-- Reducing strings (folds)
--

foldl :: (a -> Chr -> a) -> a -> Str -> a
foldl _ _ _ = u "foldl"

foldl' :: (a -> Chr -> a) -> a -> Str -> a
foldl' _ _ _ = u "foldl'"

foldl1 :: (Chr -> Chr -> Chr) -> Str -> Chr
foldl1 _ _ = u "foldl1"

foldl1' :: (Chr -> Chr -> Chr) -> Str -> Chr
foldl1' _ _ = u "foldl1'"

foldr :: (Chr -> a -> a) -> a -> Str -> a
foldr _ _ _ = u "foldr"

foldr' :: (Chr -> a -> a) -> a -> Str -> a
foldr' _ _ _ = u "foldr'"

foldr1 :: (Chr -> Chr -> Chr) -> Str -> Chr
foldr1 _ _ = u "foldr1"

foldr1' :: (Chr -> Chr -> Chr) -> Str -> Chr
foldr1' _ _ = u "foldr1'"

-- --------------------------------------------------------------------
-- Special folds
--

concat :: [Str] -> Str
concat _ = u "concat"

concatMap :: (Chr -> Str) -> Str -> Str
concatMap _ _ = u "concatMap"

any :: (Chr -> Bool) -> Str -> Bool
any _ _ = u "any"

all :: (Chr -> Bool) -> Str -> Bool
all _ _ = u "all"

maximum :: Str -> Chr
maximum _ = u "maximum"

minimum :: Str -> Chr
minimum _ = u "minimum"

-- ---------------------------------------------------------------------
-- Building Strs
--

scanl :: (Chr -> Chr -> Chr) -> Chr -> Str -> Str
scanl _ _ _ = u "scanl"

scanl1 :: (Chr -> Chr -> Chr) -> Str -> Str
scanl1 _ _ = u "scanl1"

scanr :: (Chr -> Chr -> Chr) -> Chr -> Str -> Str
scanr _ _ _ = u "scanr"

scanr1 :: (Chr -> Chr -> Chr) -> Str -> Str
scanr1 _ _ = u "scanr1"

-- --------------------------------------------------------------------
-- Accumulating maps
--

mapAccumL :: (acc -> Chr -> (acc, Chr)) -> acc -> Str -> (acc, Str)
mapAccumL _ _ _ = u "mapAccumL"

mapAccumR :: (acc -> Chr -> (acc, Chr)) -> acc -> Str -> (acc, Str)
mapAccumR _ _ _ = u "mapAccumR"

-- ---------------------------------------------------------------------
-- Infinite strings
--

repeat :: Chr -> Str
repeat _ = u "repeat"

cycle :: Str -> Str
cycle _ = u "cycle"

iterate :: (Chr -> Chr) -> Chr -> Str
iterate _ _ = u "iterate"

-- ---------------------------------------------------------------------
-- Unfolds and replicates
--

replicate :: Index -> Chr -> Str
replicate _ _ = u "replicate"

concatReplicate :: Index -> Str -> Str
concatReplicate _ _ = u "concatReplicate"

unfoldr :: (a -> Maybe (Chr, a)) -> a -> Str
unfoldr _ _ = u "unfoldr"

unfoldrN :: Int -> (a -> Maybe (Chr, a)) -> a -> (Str, Maybe a)
unfoldrN _ _ _ = u "unfoldrN"

-- ---------------------------------------------------------------------
-- Substrings: Breaking strings
--

take :: Index -> Str -> Str
take _ _ = u "take"

takeEnd :: Index -> Str -> Str
takeEnd _ _ = u "takeEnd"

drop  :: Index -> Str -> Str
drop _ _ = u "drop"

dropEnd :: Index -> Str -> Str
dropEnd _ _ = u "dropEnd"

splitAt :: Index -> Str -> (Str, Str)
splitAt _ _ = u "splitAt"

takeWhile :: (Chr -> Bool) -> Str -> Str
takeWhile _ _ = u "takeWhile"

takeWhileEnd :: (Chr -> Bool) -> Str -> Str
takeWhileEnd _ _ = u "takeWhileEnd"

dropWhile :: (Chr -> Bool) -> Str -> Str
dropWhile _ _ = u "dropWhile"

dropWhileEnd :: (Chr -> Bool) -> Str -> Str
dropWhileEnd _ _ = u "dropWhileEnd"

stripStart :: Str -> Str
stripStart _ = u "stripStart"

stripEnd :: Str -> Str
stripEnd _ = u "stripEnd"

strip :: Str -> Str
strip _ = u "strip"

span :: (Chr -> Bool) -> Str -> (Str, Str)
span _ _ = u "span"

spanEnd :: (Chr -> Bool) -> Str -> (Str, Str)
spanEnd _ _ = u "spanEnd"

break :: (Chr -> Bool) -> Str -> (Str, Str)
break _ _ = u "break"

breakEnd :: (Chr -> Bool) -> Str -> (Str, Str)
breakEnd _ _ = u "breakEnd"

breakOn :: Str -> Str -> (Str, Str)
breakOn _ _ = u "breakOn"

breakOnEnd :: Str -> Str -> (Str, Str)
breakOnEnd _ _ = u "breakOnEnd"

group :: Str -> [Str]
group _ = u "group"

groupBy :: (Chr -> Chr -> Bool) -> Str -> [Str]
groupBy _ _ = u "groupBy"

inits :: Str -> [Str]
inits _ = u "inits"

tails :: Str -> [Str]
tails _ = u "tails"

stripPrefix :: Str -> Str -> Maybe Str
stripPrefix _ _ = u "stripPrefix"

stripSuffix :: Str -> Str -> Maybe Str
stripSuffix _ _ = u "stripSuffix"

-- ---------------------------------------------------------------------
-- Substrings: Breaking into many substrings
--

splitOn :: Str -> Str -> [Str]
splitOn _ _ = u "splitOn"

splitWhen :: (Chr -> Bool) -> Str -> [Str]
splitWhen _ _ = u "splitWhen"

chunksOf :: Index -> Str -> [Str]
chunksOf _ _ = u "chunksOf"

-- ---------------------------------------------------------------------
-- Breaking into lines and words
--

lines :: Str -> [Str]
lines _ = u "lines"

unlines :: [Str] -> Str
unlines _ = u "unlines"

words :: Str -> [Str]
words _ = u "words"

unwords :: [Str] -> Str
unwords _ = u "unwords"

-- ---------------------------------------------------------------------
-- Predicates
--

isPrefixOf :: Str -> Str -> Bool
isPrefixOf _ _ = u "isPrefixOf"

isSuffixOf :: Str -> Str -> Bool
isSuffixOf _ _ = u "isSuffixOf"

isInfixOf :: Str -> Str -> Bool
isInfixOf _ _ = u "isInfixOf"

-- ---------------------------------------------------------------------
-- View patterns
--

commonPrefixes :: Str -> Str -> Maybe (Str,Str,Str)
commonPrefixes _ _ = u "commonPrefixes"

-- ---------------------------------------------------------------------
-- Search for arbitrary substrings
--

breakSubstring :: Str -> Str -> (Str,Str)
breakSubstring _ _ = u "breakSubstring"

findSubstring :: Str -> Str -> Maybe Int
findSubstring _ _ = u "findSubstring"

findSubstrings :: Str -> Str -> [Int]
findSubstrings _ _ = u "findSubstrings"

-- ---------------------------------------------------------------------
-- Searching by equality
--

elem :: Chr -> Str -> Bool
elem _ _ = u "elem"

notElem :: Chr -> Str -> Bool
notElem _ _ = u "notElem"

-- ---------------------------------------------------------------------
-- Searching with a predicate
--

find :: (Chr -> Bool) -> Str -> Maybe Chr
find _ _ = u "find"

filter :: (Chr -> Bool) -> Str -> Str
filter _ _ = u "filter"

partition :: (Chr -> Bool) -> Str -> (Str, Str)
partition _ _ = u "partition"

breakOnAll :: Str -> Str -> [(Str, Str)]
breakOnAll _ _ = u "breakOnAll"

-- ---------------------------------------------------------------------
-- Indexing Strs

index :: Str -> Index -> Chr
index _ _ = u "index"

elemIndex :: Chr -> Str -> Maybe Index
elemIndex _ _ = u "elemIndex"

elemIndices :: Chr -> Str -> [Index]
elemIndices _ _ = u "elemIndices"

elemIndexEnd :: Chr -> Str -> Maybe Index
elemIndexEnd _ _ = u "elemIndexEnd"

findIndex :: (Chr -> Bool) -> Str -> Maybe Index
findIndex _ _ = u "findIndex"

findIndices :: (Chr -> Bool) -> Str -> [Index]
findIndices _ _ = u "findIndices"

elemCount :: Chr -> Str -> Index
elemCount _ _ = u "elemCount"

substringCount :: Str -> Str -> Index
substringCount _ _ = u "substringCount"

-- ---------------------------------------------------------------------
-- Zipping and unzipping
--

zip :: Str -> Str -> [(Chr,Chr)]
zip _ _ = u "zip"

zipWith :: (Chr -> Chr -> a) -> Str -> Str -> [a]
zipWith _ _ _ = u "zipWith"

packZipWith :: (Chr -> Chr -> Chr) -> Str -> Str -> Str
packZipWith _ _ _ = u "packZipWith"

unzip :: [(Chr,Chr)] -> (Str,Str)
unzip _ = u "unzip"

-- ---------------------------------------------------------------------
-- Ordered Strs
--

sort :: Str -> Str
sort _ = u "sort"

-- ---------------------------------------------------------------------
-- Copying Strs
--

copy :: Str -> Str
copy _ = u "copy"

-- ---------------------------------------------------------------------
-- Using Strs as 'CString's
--

packCString :: CString -> IO Str
packCString _ = u "packCString"

packCStringLen :: CStringLen -> IO Str
packCStringLen _ = u "packCStringLen"

-- --------------------------------------------------------------------
-- Interpreting Str as an operating system string
--

useAsOSString :: Str -> (CString -> IO a) -> IO a
useAsOSString _ _ = u "useAsOSString"

newOSString :: Str -> IO CString
newOSString _ = u "newOSString"

packOSString :: CString -> IO Str
packOSString _ = u "packOSString"

-- --------------------------------------------------------------------
-- Reading from Str
--

readInt :: Str -> Maybe (Int, Str)
readInt _ = u "readInt"

readInteger :: Str -> Maybe (Integer, Str)
readInteger _ = u "readInteger"

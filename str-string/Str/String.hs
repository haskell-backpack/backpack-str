{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe #-}
-- | Adaptor module for 'String' that implements the Str signature.
module Str.String (
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
    -- cons',
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
    replicate,
    concatReplicate,
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
    -- breakSubstring,
    -- findSubstring,
    -- findSubstrings,

    -- * Searching by equality
    elem,
    notElem,

    -- * Searching with a predicate
    find,
    filter,
    partition,
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
    packZipWith,
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
    -- readInt,
    -- readInteger,
) where

import Prelude (id, not, (++), Int, Char, Bool, String, IO, Maybe(..), Eq(..), (.),
                Ordering, Ord(..))
import Prelude (unwords, words, lines, unlines)
import qualified Data.List as P
import qualified Prelude as P
import qualified Data.Foldable as F
import Foreign.C.String
import System.Posix.Internals
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (getFileSystemEncoding)

type Str = String
type Chr = Char
type Index = Int

-- | /O(1)/ The empty 'Str'.  This value is expected to coincide with
-- 'mempty'.
--
empty :: Str
empty = ""

-- | /O(1)/ Convert a 'Char' into a 'Str'.
singleton :: Char -> Str
singleton c = [c]

-- | /O(1)/ Convert a 'String' into a 'Str'.  Assumed to coincide with
-- 'fromString'.
--
pack :: String -> Str
pack = id

-- | /O(1)/ Convert a 'Str' into a 'String'.
--
unpack :: Str -> String
unpack = id

-- --------------------------------------------------------------------
-- Basic interface
--

-- | /O(1)/ Prepend a character onto the beginning of a 'Str'.
--
cons :: Char -> Str -> Str
cons = (:)

infixr 5 `cons`

-- | Lazy /O(n)/ Append a character onto the end of a 'Str'.
--
snoc :: Str -> Char -> Str
snoc s c = s ++ [c]

infixl 5 `snoc`

-- | Lazy /O(n)/ Append two strings.  If the first string is not finite, the result
-- is the first string.  This operation is expected to coincide with
-- 'mappend'.
--
append :: Str -> Str -> Str
append = (++)

-- | /O(1)/ Extract the first character of a string, which must be non-empty.
--
head :: Str -> Char
head = P.head

-- | /O(1)/ Extract the 'head' and 'tail' of 'Str', returning 'Nothing' if
-- it is empty.
--
uncons :: Str -> Maybe (Char, Str)
uncons []     = Nothing
uncons (c:cs) = Just (c, cs)

-- | /O(n)/ Extract the 'init' and 'last' of a string, returning
-- 'Nothing' if it is empty.  The resulting 'Str' MAY retain the input
-- argument.
--
unsnoc :: Str -> Maybe (Str, Char)
unsnoc [] = Nothing
-- TODO: since last forces the string, you get different perf
-- characteristics if you use that string to compute init.
unsnoc s  = Just (init s, last s)

-- | /O(n)/ Extract the last element of a 'Str', which must be finite
-- and non-empty.  An exception will be thrown in the case of an empty
-- 'Str'.
--
last :: Str -> Char
last = P.last

-- | /O(1)/ Extract the substring after the head of the string, which must be
-- non-empty.  The resulting 'Str' MAY retain the input argument.
--
tail :: Str -> Str
tail = P.tail

-- | Lazy /O(n)/ Return all characters of the 'Str' except the last one.
-- The resulting 'Str' MAY retain the input argument.
--
init :: Str -> Str
init = P.init

-- | /O(1)/ Test whether a 'Str' is empty.
--
null :: Str -> Bool
null = P.null

-- | /O(n)/ Return the length of a 'Str' as an 'Index'.
--
length :: Str -> Index
length = P.length

-- | /O(n)/ Compare the count of characters in a 'Str' to a number.
-- This function gives the same answer as comparing against the result
-- of 'length', but can short circuit if the count of characters is
-- greater than the number, and hence be more efficient.
--
compareLength :: Str -> Index -> Ordering
compareLength s = compare (length s)

-- --------------------------------------------------------------------
-- Transforming strings
--

-- | Lazy /O(n)/ The 'Str' obtained by applying a function to each character
-- in the string.
--
map :: (Char -> Char) -> Str -> Str
map = P.map

-- | /O(n)/ Returns the reverse of a 'Str'
--
reverse :: Str -> Str
reverse = P.reverse

-- | /O(n)/ Takes a 'Char' and a 'Str', and intersperses that character
-- between the characters of 'Str'.
--
intersperse :: Char -> Str -> Str
intersperse = P.intersperse

-- | /O(n)/ Takes a 'Str' and a list of 'Str's, and concatenates the
-- list after interspersing the first argument between each 'Str'
-- in the list.
--
intercalate :: Str -> [Str] -> Str
intercalate = P.intercalate

-- | Transposes the rows and columns of its 'Str' argument.
--
transpose :: [Str] -> [Str]
transpose = P.transpose

-- --------------------------------------------------------------------
-- Reducing strings (folds)
--

-- | Given a binary operator, a starting value (typically the
-- left-identity of the operator), and a 'Str', reduces the
-- 'Str' using the binary operator, from left to right.
--
foldl :: (a -> Char -> a) -> a -> Str -> a
foldl = P.foldl

-- | Like 'foldl', but strict in the accumulator.
--
foldl' :: (a -> Char -> a) -> a -> Str -> a
foldl' = P.foldl'

-- | A variant of 'foldl' that has no starting value argument, and
-- thus must be applied to non-empty 'Str's.  An exception will be
-- thrown in case of an empty 'Str'.
--
foldl1 :: (Char -> Char -> Char) -> Str -> Char
foldl1 = P.foldl1

-- | Like 'foldl1', but strict in the accumulator.  An exception will be
-- thrown in case of an empty 'Str'.
--
foldl1' :: (Char -> Char -> Char) -> Str -> Char
foldl1' = P.foldl1'

-- | Given a binary operator, a starting value (typically the
-- right-identity of the operator), and a 'Str', reduces the
-- 'Str' using the binary operator, from right to left.
--
foldr :: (Char -> a -> a) -> a -> Str -> a
foldr = P.foldr

-- | Like 'foldr', but strict in the accumulator.
--
foldr' :: (Char -> a -> a) -> a -> Str -> a
foldr' = F.foldr'

-- | A variant of 'foldr' that has no starting value argument, and
-- thus must be applied to non-empty 'Str's.  An exception will be
-- thrown in case of an empty 'Str'.
--
foldr1 :: (Char -> Char -> Char) -> Str -> Char
foldr1 = P.foldr1

-- --------------------------------------------------------------------
-- Special folds
--

-- | /O(n)/ Concatenate a list of 'Str's
--
concat :: [Str] -> Str
concat = P.concat

-- | Map a function over a 'Str' and concatenate the results
--
concatMap :: (Char -> Str) -> Str -> Str
concatMap = P.concatMap

-- | /O(n)/ Applied to a predicate and a 'Str', any determines if any
-- character of the 'Str' satisfies the predicate.
--
any :: (Char -> Bool) -> Str -> Bool
any = P.any

-- | /O(n)/ Applied to a predicate and a 'Str', all determines if all
-- elements of the 'Str' satisfy the predicate.
--
all :: (Char -> Bool) -> Str -> Bool
all = P.all

-- | /O(n)/ 'maximum' returns the maximum value from a 'Str'. An
-- exception will be thrown in the case of an empty 'Str'.
--
maximum :: Str -> Char
maximum = P.maximum

-- | /O(n)/ 'minimum' returns the minimum value from a 'Str'. An
-- exception will be thrown in the case of an empty 'Str'.
--
minimum :: Str -> Char
minimum = P.minimum

-- --------------------------------------------------------------------
-- Accumulating maps
--

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each character of a 'Str',
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new list.
--
mapAccumL :: (acc -> Char -> (acc, Char)) -> acc -> Str -> (acc, Str)
mapAccumL = P.mapAccumL

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each character of a 'Str',
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new 'Str'.
--
mapAccumR :: (acc -> Char -> (acc, Char)) -> acc -> Str -> (acc, Str)
mapAccumR = P.mapAccumR

-- ---------------------------------------------------------------------
-- Building Strs
--

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left.
--
scanl :: (Char -> Char -> Char) -> Char -> Str -> Str
scanl = P.scanl

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument.
--
scanl1 :: (Char -> Char -> Char) -> Str -> Str
scanl1 = P.scanl1

-- | scanr is the right-to-left dual of scanl.
--
scanr :: (Char -> Char -> Char) -> Char -> Str -> Str
scanr = P.scanr

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
--
scanr1 :: (Char -> Char -> Char) -> Str -> Str
scanr1 = P.scanr1

-- ---------------------------------------------------------------------
-- Infinite strings
--

-- | @'repeat' x@ is an infinite 'Str', with @x@ the value of every
-- element.  Not supported by strict string representations.
--
repeat :: Chr -> Str
repeat = P.repeat

-- | 'cycle' ties a finite 'Str' into a circular one, or equivalently,
-- the infinite repetition of the original 'Str'.  Not supported by
-- strict string representations.
--
cycle :: Str -> Str
cycle = P.cycle

-- | @'iterate' f x@ returns an infinite 'Str' of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]
--
iterate :: (Chr -> Chr) -> Chr -> Str
iterate = P.iterate

-- ---------------------------------------------------------------------
-- Unfolds and replicates
--

-- | Lazy /O(n)/ 'replicate' @n x@ is a 'Str' of length @n@ with @x@
-- the value of every character.
--
replicate :: Index -> Char -> Str
replicate = P.replicate

-- | /O(n)/ 'concatReplicate' @n x@ is a 'Str' consisting of @x@
-- repeated @n@ times.  This is sometimes called @replicate@.
--
-- > concatReplicate = concat . replicate
--
concatReplicate :: Index -> Str -> Str
concatReplicate n = concat . P.replicate n

-- | /O(n)/, where /n/ is the length of the result.  'unfoldr' builds a
-- 'Str' from a seed value.  The function takes the character and
-- returns 'Nothing' if it is done producing the 'Str' or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string,
-- and @b@ is the seed value for further production.
--
unfoldr :: (a -> Maybe (Char, a)) -> a -> Str
unfoldr = P.unfoldr

-- ---------------------------------------------------------------------
-- Substrings: Breaking strings
--

-- | Lazy /O(n)/ 'take' @n@, applied to a Str @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
--
take :: Index -> Str -> Str
take = P.take

-- | /O(n)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- characters, or @[]@ if @n > 'length' xs@.
--
drop  :: Index -> Str -> Str
drop = P.drop

-- | /O(n)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
-- The resulting 'Str' MAY retain the input argument.
--
splitAt :: Index -> Str -> (Str, Str)
splitAt = P.splitAt

-- | 'takeWhile', applied to a predicate @p@ and a Str @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of characters that
-- satisfy @p@. The resulting 'Str' MAY retain the input argument.
--
takeWhile :: (Char -> Bool) -> Str -> Str
takeWhile = P.takeWhile

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
-- The resulting 'Str' MAY retain the input argument.
--
dropWhile :: (Char -> Bool) -> Str -> Str
dropWhile = P.dropWhile

-- | 'span' @p xs@ breaks the 'Str' into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@.
--
span :: (Char -> Bool) -> Str -> (Str, Str)
span = P.span

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
--
break :: (Char -> Bool) -> Str -> (Str, Str)
break = P.break

-- | The 'group' function takes a Str and returns a list of
-- Strs such that the concatenation of the result is equal to the
-- argument.  Moreover, each sublist in the result contains only equal
-- characters.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test; this function may be implemented more
-- efficiently than @'groupBy' (==)@.
--
-- The resulting 'Str's MAY retain the input argument.
--
group :: Str -> [Str]
group = P.group

-- | The 'groupBy' function is the non-overloaded version of 'group'.
--
groupBy :: (Char -> Char -> Bool) -> Str -> [Str]
groupBy = P.groupBy

-- | /O(n)/ Return all initial segments of the given 'Str', shortest first.
--
inits :: Str -> [Str]
inits = P.inits

-- | /O(n)/ Return all final segments of the given 'Str', longest first.
--
tails :: Str -> [Str]
tails = P.tails

-- | /O(n)/ The 'stripPrefix' function takes two Strs and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.  The resulting 'Str's MAY retain the input argument.
--
stripPrefix :: Str -> Str -> Maybe Str
stripPrefix = P.stripPrefix

-- | /O(n)/ The 'stripSuffix' function takes two Strs and returns 'Just'
-- the remainder of the second iff the first is its suffix, and otherwise
-- 'Nothing'.  The resulting 'Str's MAY retain the input argument.
--
stripSuffix :: Str -> Str -> Maybe Str
stripSuffix ns hs = do
    delta <- dropLengthMaybe ns hs
    if ns == dropLength delta hs
        then Just delta
        else Nothing
  where
    dropLength :: [a] -> [b] -> [b]
    dropLength [] y = y
    dropLength _ [] = []
    dropLength (_:x') (_:y') = dropLength x' y'

    dropLengthMaybe :: [a] -> [b] -> Maybe [b]
    dropLengthMaybe [] y = Just y
    dropLengthMaybe _ [] = Nothing
    dropLengthMaybe (_:x') (_:y') = dropLengthMaybe x' y'

-- A version of dropLength that returns Nothing if the second list runs out of
-- elements before the first.
dropLengthMaybe :: [a] -> [b] -> Maybe [b]
dropLengthMaybe [] y = Just y
dropLengthMaybe _ [] = Nothing
dropLengthMaybe (_:x') (_:y') = dropLengthMaybe x' y'

-- ---------------------------------------------------------------------
-- Substrings: Breaking into many substrings
--

-- | /O(n)/ Splits a 'Str' into components delimited by
-- separators, where the predicate returns True for a separator character.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWhen (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWhen (=='a') []        == []
--
-- The resulting 'Str's MAY retain the input argument.
--
splitWhen :: (Char -> Bool) -> Str -> [Str]
splitWhen p s =
    let (pre, post) = break p s
    in case post of
        []     -> [pre]
        (_:s') -> pre : splitWhen p s'

-- ---------------------------------------------------------------------
-- Predicates
--

-- | /O(n)/ The 'isPrefixOf' function takes two 'Str's and returns 'True'
-- if the first is a prefix of the second.
isPrefixOf :: Str -> Str -> Bool
isPrefixOf = P.isPrefixOf

-- | /O(n)/ The 'isSuffixOf' function takes two Strs and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
isSuffixOf :: Str -> Str -> Bool
isSuffixOf = P.isSuffixOf

-- | Check whether one string is a substring of another. @isInfixOf
-- p s@ is equivalent to @not (null (findSubstrings p s))@.
--
isInfixOf :: Str -> Str -> Bool
isInfixOf = P.isInfixOf

-- ---------------------------------------------------------------------
-- Searching by equality
--

-- | /O(n)/ 'elem' is the 'Str' membership predicate.
elem :: Char -> Str -> Bool
elem = P.elem

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Char -> Str -> Bool
notElem c s = not (P.elem c s)

-- ---------------------------------------------------------------------
-- Searching with a predicate
--

-- | /O(n)/ The 'find' function takes a predicate and a Str,
-- and returns the first character in matching the predicate, or 'Nothing'
-- if there is no such character.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Char -> Bool) -> Str -> Maybe Char
find = P.find

-- | /O(n)/ 'filter', applied to a predicate and a Str,
-- returns a Str containing those characters that satisfy the
-- predicate.
--
filter :: (Char -> Bool) -> Str -> Str
filter = P.filter

-- | /O(n)/ The 'partition' function takes a predicate a Str and returns
-- the pair of Strs with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p xs, filter (not . p) xs)
--
partition :: (Chr -> Bool) -> Str -> (Str, Str)
partition = P.partition

-- ---------------------------------------------------------------------
-- Indexing Strs

-- | /O(n)/ 'Str' index (subscript) operator, starting from 0.
--
index :: Str -> Index -> Char
index = (P.!!)

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- character in the given 'Str' which is equal to the query
-- character, or 'Nothing' if there is no such character.
--
elemIndex :: Char -> Str -> Maybe Index
elemIndex = P.elemIndex

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all characters equal to the query character, in ascending order.
--
elemIndices :: Char -> Str -> [Index]
elemIndices = P.elemIndices

elemCount :: Char -> Str -> Index
elemCount c = P.length . P.elemIndices c

-- | The 'findIndex' function takes a predicate and a 'Str' and
-- returns the index of the first character in the 'Str'
-- satisfying the predicate.
--
findIndex :: (Char -> Bool) -> Str -> Maybe Index
findIndex = P.findIndex

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all characters satisfying the predicate, in ascending order.
--
findIndices :: (Char -> Bool) -> Str -> [Index]
findIndices = P.findIndices

-- | /O(n)/ count returns the number of times its argument appears in the Str
--
-- > count = length . elemIndices
--
count :: Char -> Str -> Index
count c s = length (filter (==c) s)

-- ---------------------------------------------------------------------
-- Zipping and unzipping
--

-- | /O(n)/ 'zip' takes two Strs and returns a list of
-- corresponding pairs of bytes. If one input Str is short,
-- excess characters of the longer Str are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: Str -> Str -> [(Char,Char)]
zip = P.zip

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two Strs to produce the list of
-- corresponding sums.
--
zipWith :: (Char -> Char -> a) -> Str -> Str -> [a]
zipWith = P.zipWith

packZipWith :: (Char -> Char -> Char) -> Str -> Str -> Str
packZipWith = zipWith

-- | /O(n)/ 'unzip' transforms a list of pairs of characters into a pair of
-- Strs.
--
unzip :: [(Char,Char)] -> (Str,Str)
unzip = P.unzip

-- ---------------------------------------------------------------------
-- Ordered Strs
--

-- | /O(n log(n))/ Sort the characters of a 'Str'.
sort :: Str -> Str
sort = P.sort

-- ---------------------------------------------------------------------
-- Copying Strs
--

-- | /O(n)/ A no-op, here for API compatibility with string
-- representations that may share buffers.
--
copy :: Str -> Str
copy = id

-- --------------------------------------------------------------------
-- Interpreting Str as an operating system string
--

-- | Marshal a 'Str' into a NUL terminated C string using temporary
-- storage.
--
-- * if 'Str' is Unicode, it is encoded to bytes using PEP 383
--   ('GHC.IO.Encoding.getFileSystemEncoding'), which will interpret
--   special use of surrogates as otherwise unrepresentable bytes.
--
-- * the 'Str' may /not/ contain any NUL characters. However, it may
--   optionally be NUL terminated.
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
useAsOSString :: Str -> (CString -> IO a) -> IO a
useAsOSString = withFilePath

-- | Marshal a 'Str' into a NUL terminated C string. However, it may
--   optionally be NUL terminated.
--
-- * if 'Str' is Unicode, it is encoded to bytes using PEP 383
--   ('GHC.IO.Encoding.getFileSystemEncoding'), which will interpret
--   special use of surrogates as otherwise unrepresentable bytes.
--
-- * the 'Str' may /not/ contain any NUL characters
--
-- * new storage is allocated for the C string and must be
--   explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
newOSString :: Str -> IO CString
newOSString   = newFilePath

-- | Marshal a NUL terminated C string into a 'Str'.
--
-- * if 'Str' is Unicode, we will decode using PEP 383
--   ('GHC.IO.Encoding.getFileSystemEncoding'), which decode
--   otherwise uninterpretable bytes as surrogate sequences,
--   which can be round-tripped back.
--
packOSString :: CString -> IO Str
packOSString  = peekFilePath

packCString :: CString -> IO Str
packCString = peekCString

packCStringLen :: CStringLen -> IO Str
packCStringLen = peekCStringLen

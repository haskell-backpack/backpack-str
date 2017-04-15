{-# LANGUAGE GADTs #-}
-- | Adaptor module for foundation 'String' that implements the Str signature.
module Str.Foundation (
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
    -- transpose,
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
    -- foldl1,
    -- foldl1',
    foldr,
    foldr',
    -- foldr1,
    -- foldr1',

    -- * Special folds
    concat,
    concatMap,
    any,
    all,
    maximum,
    minimum,

    -- * Building strings
    -- scanl,
    -- scanl1,
    -- scanr,
    -- scanr1,

    -- * Accumulating maps
    -- mapAccumL,
    -- mapAccumR,

    -- * Infinite strings
    -- repeat,
    -- cycle,
    -- iterate,

    -- * Unfolds and replicates
    -- replicate,
    -- concatReplicate,
    -- unfoldr,
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
    -- group,
    -- groupBy,
    -- inits,
    -- tails,

    -- * Substrings: Breaking into many substrings
    -- splitOn,
    splitWhen,
    -- chunksOf,

    -- * Breaking into lines and words
    lines,
    -- unlines,
    words,
    -- unwords,

    -- * Predicates
    isPrefixOf,
    isSuffixOf,
    -- isInfixOf,

    -- * View patterns
    stripPrefix,
    stripSuffix,
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

    -- * Indexing strings
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

    -- * Copying strings
    -- copy,

    -- * Using as CString
    -- packCString,
    -- packCStringLen,

    -- * Using as operating system string
    useAsOSString,
    newOSString,
    packOSString,

    -- * Reading integers from strings
    -- readInt,
    -- readInteger,
) where

import Prelude (id, not, Int, Char, Bool, IO, Maybe(..), Eq(..), Num(..),
                (.), undefined, otherwise)
import qualified Prelude as P
import qualified Foundation as F
import qualified Foundation.Collection as F
import Foundation.String (lines, words)
import System.Posix.Internals (withFilePath, newFilePath, peekFilePath)
import Foreign.C (CString)

-- | A packed, space-efficient, UTF-8 encoded string.
--
type Str = F.String

-- | The length and positions of characters within a 'Text' are
-- measured with machine-precision 'Int'.
--
type Index = Int

-- | The characters of a 'F.String' are Unicode characters.
--
type Chr = Char

-- | /O(1)/ The empty 'Str'.  This value is expected to coincide with
-- 'mempty'.
--
empty :: Str
empty = F.mempty

-- | /O(1)/ Convert a 'Char' into a 'Str'.
singleton :: Char -> Str
singleton c = F.fromList [c]

-- | Convert a 'String' into a 'Str'.  Assumed to coincide with
-- 'fromString'.
--
pack :: [Chr] -> Str
pack = F.fromList

-- | Convert a 'Str' into a 'String'.
--
unpack :: Str -> [Chr]
unpack = F.toList

-- --------------------------------------------------------------------
-- Basic interface
--

-- | /O(n)/ Unlike 'cons', 'cons\'' is strict in the Str that we are
-- consing onto, allowing for the possibility that the new 'Chr' can be
-- coalesced into the string.  This may be as expensive as performing a
-- full copy of the string, but may be less expensive if the string is
-- chunked; whether or not coalescing occurs is at the discretion of the
-- implementation.
--
cons' :: Chr -> Str -> Str
cons' = F.cons

infixr 5 `cons'`

-- | Append a character onto the end of a 'Str'.
--
snoc :: Str -> Char -> Str
snoc = F.snoc

infixl 5 `snoc`

-- | Append two strings.  If the first string is not finite, the result
-- is the first string.  This operation is expected to coincide with
-- 'mappend'.
--
append :: Str -> Str -> Str
append = F.mappend

-- | Extract the first character of a string, which must be non-empty.
--
head :: Str -> Char
head = F.head . F.nonEmpty_

-- | Extract the 'head' and 'tail' of 'Str', returning 'Nothing' if
-- it is empty.
--
uncons :: Str -> Maybe (Char, Str)
uncons = F.uncons

-- | Extract the 'init' and 'last' of a string, returning
-- 'Nothing' if it is empty.  The resulting 'Str' MAY retain the input
-- argument.
--
unsnoc :: Str -> Maybe (Str, Char)
unsnoc = F.unsnoc

-- | Extract the last element of a 'Str', which must be finite
-- and non-empty.  An exception will be thrown in the case of an empty
-- 'Str'.
--
last :: Str -> Char
last = F.last . F.nonEmpty_

-- | Extract the substring after the head of the string, which must be
-- non-empty.  The resulting 'Str' MAY retain the input argument.
--
tail :: Str -> Str
tail = F.tail . F.nonEmpty_

-- | Return all characters of the 'Str' except the last one.
-- The resulting 'Str' MAY retain the input argument.
--
init :: Str -> Str
init = F.init . F.nonEmpty_

-- | Test whether a 'Str' is empty.
--
null :: Str -> Bool
null = F.null

-- | Return the length of a 'Str' as an 'Index'.
--
length :: Str -> Index
length = F.length

-- --------------------------------------------------------------------
-- Transforming strings
--

-- | The 'Str' obtained by applying a function to each character
-- in the string.
--
map :: (Char -> Char) -> Str -> Str
map = F.imap

-- | Returns the reverse of a 'Str'
--
reverse :: Str -> Str
reverse = F.reverse

-- | Takes a 'Char' and a 'Str', and intersperses that character
-- between the characters of 'Str'.
--
intersperse :: Char -> Str -> Str
intersperse = F.intersperse

-- | Takes a 'Str' and a list of 'Str's, and concatenates the
-- list after interspersing the first argument between each 'Str'
-- in the list.
--
intercalate :: Str -> [Str] -> Str
intercalate = F.intercalate

-- --------------------------------------------------------------------
-- Reducing strings (folds)
--

-- | Given a binary operator, a starting value (typically the
-- left-identity of the operator), and a 'Str', reduces the
-- 'Str' using the binary operator, from left to right.
--
foldl :: (a -> Char -> a) -> a -> Str -> a
foldl f z = F.foldl f z . F.toList

-- | Like 'foldl', but strict in the accumulator.
--
foldl' :: (a -> Char -> a) -> a -> Str -> a
foldl' f z = F.foldl' f z . F.toList

-- | Given a binary operator, a starting value (typically the
-- right-identity of the operator), and a 'Str', reduces the
-- 'Str' using the binary operator, from right to left.
--
foldr :: (Char -> a -> a) -> a -> Str -> a
foldr f z = F.foldr f z . F.toList

-- | Like 'foldr', but strict in the accumulator.
--
foldr' :: (Char -> a -> a) -> a -> Str -> a
foldr' f z = F.foldr' f z . F.toList

-- --------------------------------------------------------------------
-- Special folds
--

-- | Concatenate a list of 'Str's
--
concat :: [Str] -> Str
concat = F.mconcat

-- | Map a function over a 'Str' and concatenate the results
--
concatMap :: (Char -> Str) -> Str -> Str
concatMap f = F.mconcat . F.fmap f . F.toList

-- | Applied to a predicate and a 'Str', any determines if any
-- character of the 'Str' satisfies the predicate.
--
any :: (Char -> Bool) -> Str -> Bool
any p = P.any p . F.toList

-- | Applied to a predicate and a 'Str', all determines if all
-- elements of the 'Str' satisfy the predicate.
--
all :: (Char -> Bool) -> Str -> Bool
all p = P.all p . F.toList

-- | 'maximum' returns the maximum value from a 'Str'. An
-- exception will be thrown in the case of an empty 'Str'.
--
maximum :: Str -> Char
maximum = F.maximum . F.nonEmpty_

-- | 'minimum' returns the minimum value from a 'Str'. An
-- exception will be thrown in the case of an empty 'Str'.
--
minimum :: Str -> Char
minimum = F.minimum . F.nonEmpty_

-- ---------------------------------------------------------------------
-- Substrings: Breaking strings
--

-- | 'take' @n@, applied to a Str @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
--
take :: Index -> Str -> Str
take = F.take

-- | 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- characters, or @[]@ if @n > 'length' xs@.
--
drop  :: Index -> Str -> Str
drop = F.drop

-- | 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
-- The resulting 'Str' MAY retain the input argument.
--
splitAt :: Index -> Str -> (Str, Str)
splitAt = F.splitAt

-- | 'takeWhile', applied to a predicate @p@ and a Str @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of characters that
-- satisfy @p@. The resulting 'Str' MAY retain the input argument.
--
takeWhile :: (Char -> Bool) -> Str -> Str
takeWhile p = F.fst . F.span p

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
-- The resulting 'Str' MAY retain the input argument.
--
dropWhile :: (Char -> Bool) -> Str -> Str
dropWhile p = F.snd . F.span p

-- | 'span' @p xs@ breaks the 'Str' into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@.
--
span :: (Char -> Bool) -> Str -> (Str, Str)
span = F.span

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
--
break :: (Char -> Bool) -> Str -> (Str, Str)
break = F.break

-- | The 'stripPrefix' function takes two Strs and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.  The resulting 'Str's MAY retain the input argument.
--
stripPrefix :: Str -> Str -> Maybe Str
stripPrefix pfx s
    | pfx `F.isPrefixOf` s = Just (F.drop (F.length pfx) s)
    | otherwise            = Nothing

-- | The 'stripSuffix' function takes two Strs and returns 'Just'
-- the remainder of the second iff the first is its suffix, and otherwise
-- 'Nothing'.  The resulting 'Str's MAY retain the input argument.
--
stripSuffix :: Str -> Str -> Maybe Str
stripSuffix sfx s
    | sfx `F.isSuffixOf` s = Just (F.revDrop (F.length sfx) s)
    | otherwise            = Nothing

-- ---------------------------------------------------------------------
-- Substrings: Breaking into many substrings
--

-- | Splits a 'Str' into components delimited by
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
splitWhen = F.splitOn

-- ---------------------------------------------------------------------
-- Predicates
--

-- | The 'isPrefixOf' function takes two 'Str's and returns 'True'
-- if the first is a prefix of the second.
isPrefixOf :: Str -> Str -> Bool
isPrefixOf = F.isPrefixOf

-- | The 'isSuffixOf' function takes two Strs and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
isSuffixOf :: Str -> Str -> Bool
isSuffixOf = F.isSuffixOf

-- ---------------------------------------------------------------------
-- Searching by equality
--

-- | 'elem' is the 'Str' membership predicate.
elem :: Char -> Str -> Bool
elem = F.elem

-- | 'notElem' is the inverse of 'elem'
notElem :: Char -> Str -> Bool
notElem = F.notElem

-- ---------------------------------------------------------------------
-- Searching with a predicate
--

-- | The 'find' function takes a predicate and a Str,
-- and returns the first character in matching the predicate, or 'Nothing'
-- if there is no such character.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Char -> Bool) -> Str -> Maybe Char
find = F.find

-- | 'filter', applied to a predicate and a Str,
-- returns a Str containing those characters that satisfy the
-- predicate.
--
filter :: (Char -> Bool) -> Str -> Str
filter = F.filter

-- | /O(n)/ The 'partition' function takes a predicate a Str and returns
-- the pair of Strs with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p xs, filter (not . p) xs)
--
partition :: (Chr -> Bool) -> Str -> (Str, Str)
partition = F.partition

-- ---------------------------------------------------------------------
-- Indexing Strs

-- | 'Str' index (subscript) operator, starting from 0.
-- (WARNING: implemented in a goofy way.)
--
index :: Str -> Index -> Char
index s i = head (P.snd (splitAt i s))

-- | The 'elemIndex' function returns the index of the first
-- character in the given 'Str' which is equal to the query
-- character, or 'Nothing' if there is no such character.
--
elemIndex :: Char -> Str -> Maybe Index
elemIndex c s = let (pre, post) = F.breakElem c s
                in if F.null post
                    then Nothing
                    else Just (F.length pre)

-- | The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all characters equal to the query character, in ascending order.
--
elemIndices :: Char -> Str -> [Index]
elemIndices c s = go 0 s
  where
    go i s = let (pre, post) = F.breakElem c s
             in case F.uncons post of
                    Nothing -> []
                    Just (_, post') ->
                        let i' = i + F.length pre
                        in i' : go (i'+1) post'

-- | The 'findIndex' function takes a predicate and a 'Str' and
-- returns the index of the first character in the 'Str'
-- satisfying the predicate.
--
findIndex :: (Char -> Bool) -> Str -> Maybe Index
findIndex p s = let (pre, post) = F.break p s
                in if F.null post
                    then Nothing
                    else Just (F.length pre)

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all characters satisfying the predicate, in ascending order.
--
findIndices :: (Char -> Bool) -> Str -> [Index]
findIndices p s = go 0 s
  where
    go i s = let (pre, post) = F.break p s
             in case F.uncons post of
                    Nothing -> []
                    Just (_, post') ->
                        let i' = i + F.length pre
                        in i' : go (i'+1) post'

-- | count returns the number of times its argument appears in the Str
--
-- > count = length . elemIndices
--
elemCount :: Char -> Str -> Index
elemCount c s = F.length (F.filter (==c) s)


-- ---------------------------------------------------------------------
-- Zipping and unzipping
--

-- | /O(n)/ 'zip' takes two Strs and returns a list of
-- corresponding pairs of bytes. If one input Str is short,
-- excess characters of the longer Str are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: Str -> Str -> [(Chr,Chr)]
zip = F.zip

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two Strs to produce the list of
-- corresponding sums.
--
zipWith :: (Chr -> Chr -> a) -> Str -> Str -> [a]
zipWith = F.zipWith

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

-- | Marshal a NUL terminated C string into a 'Str'.
--
-- * if 'Str' is Unicode, we will decode using PEP 383
--   ('GHC.IO.Encoding.getFileSystemEncoding'), which decode
--   otherwise uninterpretable bytes as surrogate sequences,
--   which can be round-tripped back.
--
packOSString :: CString -> IO Str

-- TODO: Do these more efficiently

useAsOSString s f = withFilePath (unpack s) f
newOSString s     = newFilePath (unpack s)
packOSString c    = P.fmap pack (peekFilePath c)

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Str.Tests.Compare (tests) where

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Text.Show.Functions

import Control.Exception

import qualified AStr as A
import qualified BStr as B

type X = Int

-- | The Model class connects a type and its model type, via a conversion
-- function.
class Model a b where
  model :: a -> b  -- get the abstract value from a concrete value

-- Must spell it this way, otherwise the other instances are not "more
-- specific" (since with Mode la a there is no substitution into them.)
instance {-# OVERLAPPABLE #-} a ~ b => Model a b where
    model = id
-- We could be more careful about this if we added stuff to signature,
-- but this is more convenient
instance Model A.Str B.Str where
    model = B.pack . A.unpack
instance Model A.Index B.Index where
    model = fromIntegral
instance Model a b => Model (Maybe a) (Maybe b) where
    model = fmap model
instance (Model f g, Model a b) => Model (f, a) (g, b) where
    model (a,b) = (model a, model b)
instance Model a b => Model [a] [b] where
    model = fmap model
instance (Model g f, Model a b) => Model (f -> a) (g -> b) where
    model x = fmap model (x . model)

------------------------------------------------------------------------
--
-- These comparison functions handle wrapping and equality.
--
-- A single class for these would be nice, but note that they differ in
-- the number of arguments, and those argument types, so we'd need HList
-- tricks. See here: http://okmij.org/ftp/Haskell/vararg-fn.lhs
--

eq0 f g =
  ioProperty $
    evaluate (model f == g)
        `catch`
    \(e :: ErrorCall) -> return True

eq1 f g = \a         ->
  ioProperty $
    evaluate (model (f a) == g (model a))
        `catch`
    \(e :: ErrorCall) -> return True
eq2 f g = \a b       ->
  ioProperty $
    evaluate (model (f a b) == g (model a) (model b))
        `catch`
    \(e :: ErrorCall) -> return True
eq3 f g = \a b c     ->
  ioProperty $
    evaluate (model (f a b c)     == g (model a) (model b) (model c))
        `catch`
    \(e :: ErrorCall) -> return True

--
-- And for functions that take non-null input
--
eqnotnull1 f g = \x     -> (not (isNull x)) ==> eq1 f g x
eqnotnull2 f g = \x y   -> (not (isNull y)) ==> eq2 f g x y
eqnotnull3 f g = \x y z -> (not (isNull z)) ==> eq3 f g x y z

class    IsNull t            where isNull :: t -> Bool
instance IsNull A.Str where isNull = A.null

-- -------------------------------------------------------------
-- The properties

tests =
    -- Instances
    [ testProperty "compare"    $ compare @A.Str    `eq2` compare @B.Str
    , testProperty "eq"         $ (==) @A.Str       `eq2` (==) @B.Str

    -- Introducing and eliminating strings
    , testProperty "empty"      $ A.empty           `eq0` B.empty
    , testProperty "singleton"  $ A.singleton `eq1`   B.singleton
    , testProperty "pack"       $ A.pack      `eq1`   B.pack
    , testProperty "unpack"     $ A.unpack    `eq1`   B.unpack

    -- Basic interface
    , testProperty "cons"      $ A.cons   `eq2`  B.cons
    , testProperty "cons'"     $ A.cons'  `eq2`  B.cons
    , testProperty "snoc"      $ A.snoc   `eq2`  B.snoc
    , testProperty "append"    $ A.append `eq2`  B.append
    , testProperty "head"      $ A.head   `eqnotnull1` B.head
    , testProperty "uncons"    $ A.uncons `eq1`  B.uncons
    , testProperty "unsnoc"    $ A.unsnoc `eq1`  B.unsnoc
    , testProperty "last"      $ A.last   `eqnotnull1` B.last
    , testProperty "tail"      $ A.tail   `eqnotnull1` B.tail
    , testProperty "init"      $ A.init   `eqnotnull1` B.init
    , testProperty "null"      $ A.null   `eq1`  B.null
    , testProperty "length"    $ A.length `eq1`  B.length
    , testProperty "compareLength" $ A.compareLength `eq2` B.compareLength

    -- Transforming strings
    , testProperty "map"         $ A.map         `eq2` B.map
    , testProperty "reverse"     $ A.reverse     `eq1` B.reverse
    , testProperty "intersperse" $ A.intersperse `eq2` B.intersperse
    , testProperty "intercalate" $ A.intercalate `eq2` B.intercalate
    , testProperty "transpose"   $ A.transpose   `eq1` B.transpose
    , testProperty "replace"     $ A.replace     `eq3` B.replace -- TODO: improve generator

    -- Case conversion
    , testProperty "toCaseFold"  $ A.toCaseFold  `eq1` B.toCaseFold
    , testProperty "toLower"     $ A.toLower     `eq1` B.toLower
    , testProperty "toUpper"     $ A.toUpper     `eq1` B.toUpper
    , testProperty "toTitle"     $ A.toTitle     `eq1` B.toTitle

    -- Justification
    , testProperty "justifyLeft"  $ A.justifyLeft   `eq3` B.justifyLeft
    , testProperty "justifyRight" $ A.justifyRight  `eq3` B.justifyRight
    , testProperty "center"       $ A.center        `eq3` B.center

    -- Reducing strings (folds)
    , testProperty "foldl"     $ A.foldl     @X `eq3` B.foldl     @X
    , testProperty "foldl'"    $ A.foldl'    @X `eq3` B.foldl'    @X
    , testProperty "foldl1"    $ A.foldl1      `eqnotnull2` B.foldl1
    , testProperty "foldl1'"   $ A.foldl1'     `eqnotnull2` B.foldl1'
    , testProperty "foldr"     $ A.foldr     @X `eq3` B.foldr     @X
    , testProperty "foldr'"    $ A.foldr'    @X `eq3` B.foldr'    @X
    , testProperty "foldr1"    $ A.foldr1      `eqnotnull2` B.foldr1
    , testProperty "foldr1'"   $ A.foldr1      `eqnotnull2` B.foldr1'

    -- Special folds
    , testProperty "concat"    $ A.concat      `eq1`  B.concat
    , testProperty "concatMap" $ forAll (sized $ \n -> resize (min 50 n) arbitrary) $
                                 A.concatMap             `eq2`  B.concatMap
    , testProperty "any"       $ A.any         `eq2`  B.any
    , testProperty "all"       $ A.all         `eq2`  B.all
    , testProperty "maximum"   $ A.maximum     `eqnotnull1` B.maximum
    , testProperty "minimum"   $ A.minimum     `eqnotnull1` B.minimum

    -- Building strings
    , testProperty "scanl"    $ A.scanl       `eqnotnull3` B.scanl
    , testProperty "scanl1"   $ A.scanl1      `eqnotnull2` B.scanl1
    , testProperty "scanr"    $ A.scanr       `eqnotnull3` B.scanr
    , testProperty "scanr1"   $ A.scanr1      `eqnotnull2` B.scanr1

    -- Accumulating maps
    , testProperty "mapAccumL" $ A.mapAccumL @X `eq3` B.mapAccumL @X
    , testProperty "mapAccumR" $ A.mapAccumR @X `eq3` B.mapAccumR @X

    -- Infinite strings
    -- NB: don't just eq these!
    -- repeat
    -- cycle
    -- iterate

    -- Unfolds and replicates
    , testProperty "replicate" $ forAll arbitrarySizedIntegral $
                                 A.replicate `eq2` B.replicate
    -- concatReplicate
    -- unfoldr
    -- unfoldrN

    -- Substrings: Breaking strings
    , testProperty "take"        $ A.take   `eq2`  B.take
    -- takeEnd
    , testProperty "drop"        $ A.drop   `eq2`  B.drop
    -- dropEnd
    , testProperty "splitAt"     $ A.splitAt `eq2`  B.splitAt
    , testProperty "takeWhile"   $ A.takeWhile             `eq2`  B.takeWhile
    -- takeWhileEnd
    , testProperty "dropWhile"   $ A.dropWhile             `eq2`  B.dropWhile
    -- dropWhileEnd
    -- stripStart
    -- stripEnd
    -- strip
    , testProperty "span"        $ A.span                  `eq2`  B.span
    -- spanEnd
    , testProperty "break"       $ A.break       `eq2`  B.break
    -- breakEnd
    -- breakOn
    -- breakOnEnd
    , testProperty "group"       $ A.group       `eq1`  B.group
    , testProperty "groupBy"     $ A.groupBy     `eq2`  B.groupBy
    , testProperty "inits"       $ A.inits       `eq1`  B.inits
    , testProperty "tails"       $ A.tails       `eq1`  B.tails
    , testProperty "stripPrefix" $ A.stripPrefix           `eq2`  B.stripPrefix
    , testProperty "stripSuffix" $ A.stripSuffix           `eq2`  B.stripSuffix

    -- Substrings: Breaking into many substrings
    , testProperty "splitOn"     $ A.splitOn               `eq2`  B.splitOn
    -- splitWhen
    -- chunksOf

    -- Breaking into lines and words
    , testProperty "lines"       $ A.lines                `eq1`  B.lines
    -- unlines
    -- words
    -- unwords

    -- Predicates
    , testProperty "isPrefixOf" $ A.isPrefixOf            `eq2`  B.isPrefixOf
    , testProperty "isSuffixOf" $ A.isSuffixOf            `eq2`  B.isSuffixOf
    , testProperty "isInfixOf"  $ A.isInfixOf  `eq2`    B.isInfixOf

    -- View patterns
    -- commonPrefixes

    -- Search for arbitrary substrings
    -- breakSubstring
    -- findSubstring
    -- findSubstrings

    -- Searching by equality
    , testProperty "elem"      $ A.elem                  `eq2`  B.elem
    , testProperty "notElem"   $ A.notElem               `eq2`  B.notElem

    -- Searching by predicate
    , testProperty "find"      $ A.find                  `eq2`  B.find
    , testProperty "filter"    $ A.filter                `eq2`  B.filter
    , testProperty "partition" $ A.partition  `eq2`    B.partition
    -- breakOnAll

    -- Indexing Strs
    -- index
    -- , testProperty "findIndex"   prop_findIndex
    -- , testProperty "findIndices" prop_findIndices
    -- elemIndexEnd
    -- , testProperty "unfoldr"     prop_unfoldr
    -- , testProperty "iterate"     prop_iterate
    -- , testProperty "repeat"      prop_repeat
    -- , testProperty "elemIndex"   prop_elemIndex
    -- , testProperty "elemIndices" prop_elemIndices
    -- substringCount

    -- Zipping and unzipping
    , testProperty "zip"     $ A.zip        `eq2`   B.zip
    , testProperty "zipWith" $ A.zipWith @X `eq3` B.zipWith @X
    -- packZipWith
    , testProperty "unzip"   $ A.unzip      `eq1`   B.unzip

    -- Ordered Strs
    -- sort

    -- Copying Strs
    -- copy

    -- Using Strs as 'CString's
    -- packCString
    -- packCStringLen

    -- Interpreting Str as an operating system string
    -- useAsOSString
    -- newOSString
    -- packOSString

    -- Reading from Str
    , testProperty "readInt"   $ A.readInt              `eq1`  B.readInt
    -- readInteger

    ]
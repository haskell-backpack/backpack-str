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
instance Model A.Str B.Str where
    model = B.pack . A.unpack
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

prop_concat       = A.concat                `eq1`  B.concat
prop_null         = A.null                  `eq1`  B.null
prop_reverse      = A.reverse               `eq1`  B.reverse
prop_transpose    = A.transpose             `eq1`  B.transpose
prop_group        = A.group                 `eq1`  B.group
prop_groupBy      = A.groupBy               `eq2`  B.groupBy
prop_inits        = A.inits                 `eq1`  B.inits
prop_tails        = A.tails                 `eq1`  B.tails
prop_all          = A.all                   `eq2`  B.all
prop_any          = A.any                   `eq2`  B.any
prop_append       = A.append                `eq2`  B.append
prop_break        = A.break                 `eq2`  B.break
{-
prop_concatMap    = forAll (sized $ \n -> resize (min 50 n) arbitrary) $
                    A.concatMap             `eq2`  B.concatMap
-}
prop_cons         = A.cons                  `eq2`  B.cons
prop_cons'        = A.cons'                 `eq2`  B.cons
prop_uncons       = A.uncons                `eq1`  B.uncons
prop_unsnoc       = A.unsnoc                `eq1`  B.unsnoc
-- prop_count        = A.count                 `eq2`  ((toInt64 .) . B.count)
-- prop_drop         = (A.drop . toInt64)      `eq2`  B.drop
prop_dropWhile    = A.dropWhile             `eq2`  B.dropWhile
prop_filter       = A.filter                `eq2`  B.filter
prop_find         = A.find                  `eq2`  B.find
-- prop_findIndex    = A.findIndex             `eq2`  ((fmap toInt64 .) . B.findIndex)
-- prop_findIndices  = A.findIndices           `eq2`  ((fmap toInt64 .) . B.findIndices)
prop_isPrefixOf   = A.isPrefixOf            `eq2`  B.isPrefixOf
prop_stripPrefix  = A.stripPrefix           `eq2`  B.stripPrefix
prop_isSuffixOf   = A.isSuffixOf            `eq2`  B.isSuffixOf
prop_stripSuffix  = A.stripSuffix           `eq2`  B.stripSuffix
prop_map          = A.map                   `eq2`  B.map
-- prop_replicate    = forAll arbitrarySizedIntegral $
--                       (A.replicate . toInt64) `eq2`  B.replicate
prop_snoc         = A.snoc                  `eq2`  B.snoc
prop_span         = A.span                  `eq2`  B.span
prop_splitOn      = A.splitOn               `eq2`  B.splitOn
-- prop_splitAt      = (A.splitAt . toInt64)   `eq2`  B.splitAt
-- prop_take         = (A.take    . toInt64)   `eq2`  B.take
prop_takeWhile    = A.takeWhile             `eq2`  B.takeWhile
prop_elem         = A.elem                  `eq2`  B.elem
prop_notElem      = A.notElem               `eq2`  B.notElem
-- prop_elemIndex    = A.elemIndex             `eq2`  ((fmap toInt64 .) . B.elemIndex)
-- prop_elemIndices  = A.elemIndices           `eq2`  ((fmap toInt64 .) . B.elemIndices)
-- prop_length       = A.length                `eq1`  (toInt64 . B.length)

prop_head         = A.head        `eqnotnull1` B.head
prop_init         = A.init        `eqnotnull1` B.init
prop_last         = A.last        `eqnotnull1` B.last
prop_maximum      = A.maximum     `eqnotnull1` B.maximum
prop_minimum      = A.minimum     `eqnotnull1` B.minimum
prop_tail         = A.tail        `eqnotnull1` B.tail
prop_foldl1       = A.foldl1      `eqnotnull2` B.foldl1
prop_foldl1'      = A.foldl1'     `eqnotnull2` B.foldl1'
prop_foldr1       = A.foldr1      `eqnotnull2` B.foldr1
prop_foldr1'      = A.foldr1      `eqnotnull2` B.foldr1'
prop_scanl        = A.scanl       `eqnotnull3` B.scanl
prop_scanl1       = A.scanl1      `eqnotnull2` B.scanl1
prop_scanr        = A.scanr       `eqnotnull3` B.scanr
prop_scanr1       = A.scanr1      `eqnotnull2` B.scanr1

prop_intersperse = A.intersperse  `eq2` B.intersperse

prop_foldl     = A.foldl     @X `eq3` B.foldl     @X
prop_foldl'    = A.foldl'    @X `eq3` B.foldl'    @X
prop_foldr     = A.foldr     @X `eq3` B.foldr     @X
prop_foldr'    = A.foldr'    @X `eq3` B.foldr'    @X
prop_mapAccumL = A.mapAccumL @X `eq3` B.mapAccumL @X
prop_mapAccumR = A.mapAccumR @X `eq3` B.mapAccumR @X

prop_readInt      = A.readInt              `eq1`  B.readInt
prop_lines        = A.lines                `eq1`  B.lines

prop_eq        = eq2
    ((==) :: A.Str -> A.Str -> Bool)
    ((==) :: B.Str -> B.Str -> Bool)
prop_compare   = eq2
    ((compare) :: A.Str -> A.Str -> Ordering)
    ((compare) :: B.Str -> B.Str -> Ordering)

{-
prop_unfoldr   =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a -> A.take (fromIntegral n) $
        A.unfoldr    f a) :: Int -> (X -> Maybe (Chr,X)) -> X -> A.Str)
    ((\n f a ->                     fst $
        P.unfoldrN n f a) :: Int -> (X -> Maybe (Chr,X)) -> X -> B.Str)

prop_unfoldr2BP   =
  forAll arbitrarySizedIntegral $ \n ->
  forAll arbitrarySizedIntegral $ \a ->
  eq2
    ((\n a -> P.take (n*100) $
        P.unfoldr    (\x -> if x <= (n*100) then Just (fromIntegral x, x + 1) else Nothing) a)
                :: Int -> Int -> P)
    ((\n a ->                     fst $
        P.unfoldrN (n*100) (\x -> if x <= (n*100) then Just (fromIntegral x, x + 1) else Nothing) a)
                :: Int -> Int -> P)
    n a

prop_unfoldr2CP   =
  forAll arbitrarySizedIntegral $ \n ->
  forAll arbitrarySizedIntegral $ \a ->
  eq2
    ((\n a -> C.take (n*100) $
        C.unfoldr    (\x -> if x <= (n*100) then Just (chr (x `mod` 256), x + 1) else Nothing) a)
                :: Int -> Int -> P)
    ((\n a ->                     fst $
        C.unfoldrN (n*100) (\x -> if x <= (n*100) then Just (chr (x `mod` 256), x + 1) else Nothing) a)
                :: Int -> Int -> P)
    n a


prop_unfoldrLC   =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a -> LC.take (fromIntegral n) $
        LC.unfoldr    f a) :: Int -> (X -> Maybe (Char,X)) -> X -> B)
    ((\n f a ->                     fst $
        C.unfoldrN n f a) :: Int -> (X -> Maybe (Char,X)) -> X -> P)
-}

{-
prop_cycleLC  a   =
  not (LC.null a) ==>
  forAll arbitrarySizedIntegral $
  eq1
    ((\n   -> LC.take (fromIntegral n) $
              LC.cycle a
     ) :: Int -> B)

    ((\n   -> LC.take (fromIntegral (n::Int)) . LC.concat $
              unfoldr (\x ->  Just (x,x) ) a
     ) :: Int -> B)


prop_iterateLC =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a -> LC.take (fromIntegral n) $
        LC.iterate  f a) :: Int -> (Char -> Char) -> Char -> B)
    ((\n f a -> fst $
        C.unfoldrN n (\a -> Just (f a, f a)) a) :: Int -> (Char -> Char) -> Char -> P)

prop_iterateLC_2   =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a -> LC.take (fromIntegral n) $
        LC.iterate  f a) :: Int -> (Char -> Char) -> Char -> B)
    ((\n f a -> LC.take (fromIntegral n) $
        LC.unfoldr (\a -> Just (f a, f a)) a) :: Int -> (Char -> Char) -> Char -> B)

prop_iterateL   =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a -> L.take (fromIntegral n) $
        L.iterate  f a) :: Int -> (W -> W) -> W -> B)
    ((\n f a -> fst $
        P.unfoldrN n (\a -> Just (f a, f a)) a) :: Int -> (W -> W) -> W -> P)

prop_repeatLC   =
  forAll arbitrarySizedIntegral $
  eq2
    ((\n a -> LC.take (fromIntegral n) $
        LC.repeat a) :: Int -> Char -> B)
    ((\n a -> fst $
        C.unfoldrN n (\a -> Just (a, a)) a) :: Int -> Char -> P)

prop_repeatL   =
  forAll arbitrarySizedIntegral $
  eq2
    ((\n a -> L.take (fromIntegral n) $
        L.repeat a) :: Int -> W -> B)
    ((\n a -> fst $
        P.unfoldrN n (\a -> Just (a, a)) a) :: Int -> W -> P)
-}

prop_partition  = A.partition  `eq2`    B.partition
prop_isInfixOf  = A.isInfixOf  `eq2`    B.isInfixOf
prop_zip        = A.zip        `eq2`   B.zip
prop_unzip      = A.unzip      `eq1`   B.unzip

prop_zipWith    = A.zipWith @X `eq3` B.zipWith @X

tests =
    [ testProperty "all"         prop_all
    , testProperty "any"         prop_any
    , testProperty "append"      prop_append
    , testProperty "compare"     prop_compare
    , testProperty "concat"      prop_concat
    , testProperty "cons"        prop_cons
    , testProperty "eq"          prop_eq
    , testProperty "filter"      prop_filter
    , testProperty "find"        prop_find
    -- , testProperty "findIndex"   prop_findIndex
    -- , testProperty "findIndices" prop_findIndices
    , testProperty "foldl"       prop_foldl
    , testProperty "foldl'"      prop_foldl'
    , testProperty "foldl1"      prop_foldl1
    , testProperty "foldl1'"     prop_foldl1'
    , testProperty "foldr"       prop_foldr
    , testProperty "foldr1"      prop_foldr1
    , testProperty "mapAccumL"   prop_mapAccumL
    , testProperty "mapAccumR"   prop_mapAccumR
    -- , testProperty "unfoldr"     prop_unfoldr
    -- , testProperty "iterate"     prop_iterate
    -- , testProperty "repeat"      prop_repeat
    , testProperty "head"        prop_head
    , testProperty "init"        prop_init
    , testProperty "isPrefixOf"  prop_isPrefixOf
    , testProperty "isSuffixOf"  prop_isSuffixOf
    , testProperty "stripPrefix" prop_stripPrefix
    , testProperty "stripSuffix" prop_stripSuffix
    , testProperty "last"        prop_last
    -- , testProperty "length"      prop_length
    , testProperty "map"         prop_map
    , testProperty "maximum"     prop_maximum
    , testProperty "minimum"     prop_minimum
    , testProperty "null"        prop_null
    , testProperty "reverse"     prop_reverse
    , testProperty "snoc"        prop_snoc
    , testProperty "tail"        prop_tail
    , testProperty "transpose"   prop_transpose
    -- , testProperty "replicate"   prop_replicate
    -- , testProperty "take"        prop_take
    -- , testProperty "drop"        prop_drop
    -- , testProperty "splitAt"     prop_splitAt
    , testProperty "takeWhile"   prop_takeWhile
    , testProperty "dropWhile"   prop_dropWhile
    , testProperty "break"       prop_break
    , testProperty "span"        prop_span
    , testProperty "group"       prop_group
    , testProperty "groupBy"     prop_groupBy
    , testProperty "inits"       prop_inits
    , testProperty "tails"       prop_tails
    , testProperty "elem"        prop_elem
    , testProperty "notElem"     prop_notElem
    , testProperty "lines"       prop_lines
    -- , testProperty "elemIndex"   prop_elemIndex
    -- , testProperty "elemIndices" prop_elemIndices
    -- , testProperty "concatMap"   prop_concatMap
    ]

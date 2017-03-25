{-# LANGUAGE FlexibleInstances #-}
module Str.Text.Instances where

import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary
    shrink = map T.pack . shrink . T.unpack

instance Arbitrary TL.Text where
    arbitrary = (TL.fromChunks . map notEmpty) `fmap` smallArbitrary
    shrink = map TL.pack . shrink . TL.unpack

smallArbitrary :: (Arbitrary a, Show a) => Gen a
smallArbitrary = sized $ \n -> resize (smallish n) arbitrary
  where smallish = round . (sqrt :: Double -> Double) . fromIntegral . abs

newtype NotEmpty a = NotEmpty { notEmpty :: a }
    deriving (Eq, Ord)

instance Show a => Show (NotEmpty a) where
    show (NotEmpty a) = show a

instance Functor NotEmpty where
    fmap f (NotEmpty a) = NotEmpty (f a)

instance Arbitrary a => Arbitrary (NotEmpty [a]) where
    arbitrary   = sized (\n -> NotEmpty `fmap` (choose (1,n+1) >>= vector))
    shrink      = shrinkNotEmpty null

instance Arbitrary (NotEmpty T.Text) where
    arbitrary   = (fmap T.pack) `fmap` arbitrary
    shrink      = shrinkNotEmpty T.null

instance Arbitrary (NotEmpty TL.Text) where
    arbitrary   = (fmap TL.pack) `fmap` arbitrary
    shrink      = shrinkNotEmpty TL.null

shrinkNotEmpty :: Arbitrary a => (a -> Bool) -> NotEmpty a -> [NotEmpty a]
shrinkNotEmpty isNull (NotEmpty xs) =
  [ NotEmpty xs' | xs' <- shrink xs, not (isNull xs') ]

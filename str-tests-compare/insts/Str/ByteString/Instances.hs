module Str.ByteString.Instances where

import Test.QuickCheck
import qualified Data.ByteString as P
import qualified Data.ByteString.Lazy as L

-- NB: We have to centralize these instances because ByteString
-- type is shared between Word8 and Char8 interfaces; if we define
-- it at each actual module we'll end up with duplicates.

sizedByteString n = do m <- choose (0, n)
                       fmap P.pack $ vectorOf m arbitrary

instance Arbitrary P.ByteString where
  arbitrary = do
    bs <- sized sizedByteString
    n  <- choose (0, 2)
    return (P.drop n bs) -- to give us some with non-0 offset

instance CoArbitrary P.ByteString where
  coarbitrary s = coarbitrary (P.unpack s)

instance Arbitrary L.ByteString where
  arbitrary = sized $ \n -> do numChunks <- choose (0, n)
                               if numChunks == 0
                                   then return L.empty
                                   else fmap (L.fromChunks .
                                              filter (not . P.null)) $
                                            vectorOf numChunks
                                                     (sizedByteString
                                                          (n `div` numChunks))

instance CoArbitrary L.ByteString where
  coarbitrary s = coarbitrary (L.unpack s)

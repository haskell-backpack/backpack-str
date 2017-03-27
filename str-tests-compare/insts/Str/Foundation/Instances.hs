module Str.Foundation.Instances where

import Test.QuickCheck
import qualified Foundation as F
import qualified Foundation.Collection as F

instance Arbitrary F.String where
    arbitrary = F.fromList `fmap` arbitrary
    shrink = map F.fromList . shrink . F.toList

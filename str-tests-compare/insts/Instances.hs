{-# LANGUAGE CPP #-}
module Instances where
import Str.ByteString.Instances
import Str.Text.Instances
#ifdef MIN_VERSION_foundation
import Str.Foundation.Instances
#endif

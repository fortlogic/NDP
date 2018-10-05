{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module NDP.Tests where

import Test.Hspec

import NDP.Primitive.ClockStrobe
import NDP.Tests.Clash
import NDP.Tests.Primitive.TH
import qualified NDP.Tests.Primitive.ClockStrobe

ndpTests :: Spec
ndpTests = do
  $(mkPrimitiveTestSuite "Custom VHDL Primitives"
     [ 'clockStrobe# ])
  clashTests "Clash"



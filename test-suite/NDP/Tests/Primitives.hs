{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module NDP.Tests.Primitives ( primitiveTests ) where

import Test.Hspec

import NDP.Primitive.ClockStrobe
import NDP.Tests.Primitive.TH
import NDP.Tests.Primitive.ClockStrobe

primitiveTests :: String -> Spec
primitiveTests name = describe name $ do
  $(testPrimitiveNamed 'clockStrobe#)

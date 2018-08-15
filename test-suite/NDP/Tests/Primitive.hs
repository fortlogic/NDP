{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module NDP.Tests.Primitive ( primitiveTests ) where

import Test.Hspec

import NDP.Primitive.ClockStrobe
import NDP.Tests.Primitive.TH
import qualified NDP.Tests.Primitive.ClockStrobe
import qualified NDP.Tests.Primitive.DCM_CLKGEN
import qualified NDP.Tests.Primitive.DCM_SP
import Xilinx.Primitive

primitiveTests :: String -> Spec
primitiveTests name = describe name $ do
  describe "Xilinx Primitives" $ do
    $(testPrimitiveNamed 'dcm_clkgen#)
    $(testPrimitiveNamed 'dcm_sp#)
  describe "Custom VHDL Primitives" $ do
    $(testPrimitiveNamed 'clockStrobe#)

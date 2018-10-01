{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module NDP.Tests where

import Test.Hspec

import NDP.Primitive.ClockStrobe
import NDP.Tests.Clash
import NDP.Tests.Primitive.TH
import Xilinx.Primitive
import qualified NDP.Tests.Primitive.ClockStrobe
import qualified NDP.Tests.Primitive.DCM_CLKGEN
import qualified NDP.Tests.Primitive.DCM_SP

ndpTests :: Spec
ndpTests = do
  $(mkPrimitiveTestSuite "Xilinx Primitives"
    [ 'dcm_clkgen#
     , 'dcm_sp# ])
  $(mkPrimitiveTestSuite "Custom VHDL Primitives"
     [ 'clockStrobe# ])
  clashTests "Clash"



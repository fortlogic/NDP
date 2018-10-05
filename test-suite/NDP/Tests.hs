{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module NDP.Tests where

import Test.Hspec

import NDP.Tests.Clash
import NDP.Tests.Primitive.TH
import Xilinx.Primitive
import qualified NDP.Tests.Primitive.DCM_CLKGEN
import qualified NDP.Tests.Primitive.DCM_SP

ndpTests :: Spec
ndpTests = do
  $(mkPrimitiveTestSuite "Xilinx Primitives"
    [ 'dcm_clkgen#
     , 'dcm_sp# ])
  clashTests "Clash"



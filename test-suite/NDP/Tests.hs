module NDP.Tests where

import Test.Hspec

import NDP.Tests.Clash
import NDP.Tests.Primitive

ndpTests :: Spec
ndpTests = do
  primitiveTests "Clash Primitives"
  clashTests "Clash"
  describe "CLaSH Testbenches" $ do
    it "should run hardware testbenches in Haskell" $ do
      pendingWith "waiting on testbenches"
  describe "VHDL Testbenches" $ do
    it "should run hardware testbenches in GHDL" $ do
      pendingWith "Waiting on testbenches and GHDL infrastructure"



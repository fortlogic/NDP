module NDP.Tests where

import Test.Hspec

import NDP.Tests.Clash

ndpTests :: Spec
ndpTests = do
  clashTests "CLaSH Tests"
  describe "CLaSH Testbenches" $ do
    it "should run hardware testbenches in Haskell" $ do
      pendingWith "waiting on testbenches"
  describe "VHDL Testbenches" $ do
    it "should run hardware testbenches in GHDL" $ do
      pendingWith "Waiting on testbenches and GHDL infrastructure"



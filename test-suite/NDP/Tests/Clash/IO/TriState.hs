module NDP.Tests.Clash.IO.TriState (tristateTests) where

import Clash.Prelude
import Test.Hspec

import NDP.IO.TriState

tristateTests :: String -> Spec
tristateTests name = describe name $ do
  describe "Datatypes" $ do
    describe "in2bit" $ do
      it "LowI" $ do
        in2bit LowI `shouldBe` low
      it "HighI" $ do
        in2bit HighI `shouldBe` high
  describe "Tristate <---> IOBUF interface" $ do
    it "should have tests" $ do
      pending
  describe "Error checking" $ do
    it "should have tests" $ do
      pending

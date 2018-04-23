module Tests.Main (main) where

import Test.Hspec

import Tests.CLaSH

main = hspec $ do
  clashTests "CLaSH Tests"
  describe "CLaSH Testbenches" $ do
    it "should run hardware testbenches in Haskell" $ do
      pendingWith "waiting on testbenches"
  describe "VHDL Testbenches" $ do
    it "should run hardware testbenches in GHDL" $ do
      pendingWith "Waiting on testbenches and GHDL infrastructure"



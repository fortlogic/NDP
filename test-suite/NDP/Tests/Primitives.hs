module NDP.Tests.Primitives ( primitiveTests ) where

import Test.Hspec

primitiveTests :: String -> Spec
primitiveTests name = describe name $ do
  describe "Clash Simulation" $ do
    it "should simulate stuff correctly" $ do
      pendingWith "TODO"
  describe "Generated VHDL" $ do
    it "should generate VHDL" $ do
      pendingWith "TODO"
    it "should simulated generated VHDL correctly" $ do
      pendingWith "TODO"
  it "should have tests" $ do
    pendingWith "TODO"


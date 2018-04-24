module NDP.Tests.Clash (clashTests) where

import Test.Hspec

import NDP.Tests.TMDS
import NDP.Tests.IO.TriState

clashTests :: String -> Spec
clashTests name = describe name $ do
  tmdsTests "TMDS"
  tristateTests "Tri-state IO"
  describe "Clocking" $ do
    it "should have tests" $ do
      pendingWith "Procrastination"

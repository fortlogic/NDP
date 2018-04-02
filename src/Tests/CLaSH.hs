module Tests.CLaSH (clashTests) where

import Test.Hspec

import Tests.TMDS
import Tests.IO.TriState
import Tests.ALU

clashTests name = describe name $ do
  tmdsTests "TMDS"
  tristateTests "Tri-state IO"
  describe "Clocking" $ do
    it "should have tests" $ do
      pendingWith "Procrastination"

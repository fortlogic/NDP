module Tests.CLaSH (clashTests) where

import Test.Hspec

import Tests.TMDS

clashTests name = describe name $ do
  tmdsTests "TMDS"
  describe "Clocking" $ do
    it "should have tests" $ do
      pendingWith "Procrastination"

module CLaSHTests (clashTests) where

import Test.Hspec

import TMDSTests

clashTests name = describe name $ do
  tmdsTests "TMDS"

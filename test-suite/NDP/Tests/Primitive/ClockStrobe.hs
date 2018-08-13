module NDP.Tests.Primitive.ClockStrobe ( qc ) where

import Test.Hspec

qc :: String -> Spec
qc name = describe name $ do
  it "does stuff" $ do
    pendingWith "huh"

module NDP.Tests.Primitive.DCM_SP ( properties ) where

import Test.Hspec

properties :: String -> Spec
properties name = describe name $ do
  it "does stuff" $ do
    pendingWith "huh"

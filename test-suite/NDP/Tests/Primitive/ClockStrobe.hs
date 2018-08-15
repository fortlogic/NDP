module NDP.Tests.Primitive.ClockStrobe ( properties ) where

-- import Clash.Prelude
import Test.Hspec

-- import NDP.Primitive.ClockStrobe

-- type SlowD = 'Dom "Slow"

properties :: String -> Spec
properties name = describe name $ do
  it "does stuff" $ do
    pendingWith "huh"

module NDP.Tests.Clash ( clashTests ) where

import Test.Tasty

import NDP.Tests.Clash.Clocking
import NDP.Tests.Clash.IO.TriState
import NDP.Tests.Clash.TMDS

clashTests :: TestTree
clashTests = testGroup "Clash"
  [ tmdsTests
  , tristateTests
  , clockingTests ]
  -- describe "Clocking" $ do
  --   it "should have tests" $ do
  --     pendingWith "Procrastination"

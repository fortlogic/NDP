module NDP.Tests.Clash ( clashTests ) where

import Test.Tasty

import NDP.Tests.TMDS
import NDP.Tests.IO.TriState

clashTests :: TestTree
clashTests = testGroup "Clash"
  [ tmdsTests
  , tristateTests
  , clockingTests ]
  -- tmdsTests "TMDS"
  -- tristateTests "Tri-state IO"
  -- describe "Clocking" $ do
  --   it "should have tests" $ do
  --     pendingWith "Procrastination"

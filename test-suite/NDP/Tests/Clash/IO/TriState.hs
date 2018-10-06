module NDP.Tests.Clash.IO.TriState ( tristateTests ) where

import Clash.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Pending

import NDP.IO.TriState

tristateTests :: TestTree
tristateTests = testGroup "Tristate IO"
  [ in2bitTests
  , tristateIOBUFTests
  , errorCheckingTests ]

in2bitTests :: TestTree
in2bitTests = testGroup "in2bit"
  [ testCase "LowI" (in2bit LowI @?= low)
  , testCase "HighI" (in2bit HighI @?= high)
  ]

tristateIOBUFTests :: TestTree
tristateIOBUFTests = "Tristate <---> IOBUF interface" `pending` "Need to write test cases"

errorCheckingTests :: TestTree
errorCheckingTests = "Error checking" `pending` "Need to write test cases"

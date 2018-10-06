{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module NDP.Tests where

import Test.Tasty

import NDP.Tests.Clash
import NDP.Tests.HDL

ndpTests :: TestTree
ndpTests = testGroup "NDP"
  [ clashTests
  , hdlTests ]



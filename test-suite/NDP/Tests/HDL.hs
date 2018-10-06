{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module NDP.Tests.HDL where

import Test.Tasty

hdlTests :: TestTree
hdlTests = testGroup "HDL"
  [ vhdlTests
  , verilogTests ]

vhdlTests :: TestTree
vhdlTests = testGroup "VHDL"
  []

verilogTests :: TestTree
verilogTests = testGroup "Verilog"
  []

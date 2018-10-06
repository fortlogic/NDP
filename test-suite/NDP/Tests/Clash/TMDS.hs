{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module NDP.Tests.Clash.TMDS ( tmdsTests ) where

import Clash.Prelude hiding ( map, (++) )
import Formatting
import Formatting.ShortFormatters
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import NDP.Video.TMDS

tmdsTests :: TestTree
tmdsTests = testGroup "TMDS"
  [ testProperty "xorEncode and xorDecode are inverses" $
    \ byte -> (byte :: BitVector 16) == (xorEncode . xorDecode) byte
  , testProperty "xnorEncode and xnorDecode are inverses" $
    \ byte -> (byte :: BitVector 16) == (xnorEncode . xnorDecode) byte
  , decodeByteCases
  ]

decodeByteCases :: TestTree
decodeByteCases = testGroup "decodeByte" (map toCases bytePairs)
  where toCases (tmds,byte) = let msg = (show tmds ++ " -> " ++ show byte) in
                                testCase msg $ (decodeByte tmds) @?= (pack byte)
        bytePairs :: [(BitVector 10, Unsigned 8)]
        bytePairs = [ ( $$(bLit "0111110000"), 0x10 )
                    , ( $$(bLit "0001001111"), 0x2F )
                    , ( $$(bLit "0111001100"), 0x54 )
                    , ( $$(bLit "0010001111"), 0x6F )
                    , ( $$(bLit "0000101111"), 0x8F )
                    , ( $$(bLit "1000111001"), 0xB4 )
                    , ( $$(bLit "1000011011"), 0xD2 )
                    , ( $$(bLit "1011110000"), 0xEF ) ]

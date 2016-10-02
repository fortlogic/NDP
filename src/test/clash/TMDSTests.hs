{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module TMDSTests (tmdsTests) where

import CLaSH.Prelude
import Formatting
import Formatting.ShortFormatters
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import TMDS

tmdsTests name = describe name $ do
  it "should be idempotent" $ property $
    \ byte dc -> byte == (decodeByte $ snd $ encodeByte dc byte)
  describe "Decoder" $ do
    itShouldDecode $$(bLit "0111110000") 0x10
    itShouldDecode $$(bLit "0001001111") 0x2F
    itShouldDecode $$(bLit "0111001100") 0x54
    itShouldDecode $$(bLit "0010001111") 0x6F
    itShouldDecode $$(bLit "0000101111") 0x8F
    itShouldDecode $$(bLit "1000111001") 0xB4
    itShouldDecode $$(bLit "1000011011") 0xD2
    itShouldDecode $$(bLit "1011110000") 0xEF

itShouldDecode :: BitVector 10 -> Unsigned 8 -> SpecWith ()
itShouldDecode word byte = do
  let label = formatToString ("should decode " % sh % " to " % sh) word byte
  it label $ do
    (decodeByte word) `shouldBe` pack byte

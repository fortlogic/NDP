{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module NDP.Video.TMDS (TMDS (TMDSData, TMDSControl),
                 tmdsEncoder,
                 encodeTMDS,
                 encodeByte,
                 decodeByte,
                 xorEncode,
                 xnorEncode,
                 xorDecode,
                 xnorDecode) where

import Clash.Prelude

import NDP.Clocking.Domains
import NDP.Utils

data TMDS = TMDSData (Unsigned 8)
          | TMDSControl (BitVector 2)
          deriving (Show, Eq)

tmdsEncoder :: HiddenClockReset domain gated synchronous
            => Signal domain TMDS
            -> Signal domain (BitVector 10)
tmdsEncoder = mealy encodeTMDS 0

encodeTMDS :: Signed 4 -> TMDS -> (Signed 4, BitVector 10)
encodeTMDS dc (TMDSData byte) = encodeByte dc (pack byte)
encodeTMDS dc (TMDSControl 0) = (0, $$(bLit "1101010100"))
encodeTMDS dc (TMDSControl 1) = (0, $$(bLit "0010101011"))
encodeTMDS dc (TMDSControl 2) = (0, $$(bLit "0101010100"))
encodeTMDS dc (TMDSControl 3) = (0, $$(bLit "1010101011"))
encodeTMDS _  (TMDSControl _) = error "impossible control word"

xnor :: Bits a => a -> a -> a
xnor a b = complement (xor a b)

xorEncode :: KnownNat n => BitVector n -> BitVector n
xorEncode = v2bv . postscanr xor 0 . bv2v

xnorEncode :: KnownNat n => BitVector n -> BitVector n
xnorEncode = v2bv . postscanr xnor 1 . bv2v

xorDecode :: KnownNat n => BitVector n -> BitVector n
xorDecode = v2bv . snd . mapAccumR iter low . bv2v
  where iter prev this = (this, prev `xor` this)

xnorDecode :: KnownNat n => BitVector n -> BitVector n
xnorDecode = v2bv . snd . mapAccumR iter high . bv2v
  where iter prev this = (this, prev `xnor` this)

transitionCount :: (KnownNat (2 ^ n),
                     KnownNat n) =>
                    BitVector (2 ^ n) -> Unsigned n
transitionCount bv = (snd . foldl acc (msb bv, 0) . bv2v) bv
  where acc (b, total) b' = (b', total + if b == b' then 0 else 1)

dcOffset :: (KnownNat (2 ^ n),
            KnownNat (n + 1)) =>
            BitVector (2 ^ n) -> Signed (n + 1)
dcOffset = foldl step 0 . map (/=low) . bv2v
  where step acc True = acc+1
        step acc False = acc-1

onesCount :: BitVector 8 -> Unsigned 4
onesCount = foldl (+) 0 . map conv . bv2v
  where conv 0 = 0
        conv 1 = 1

bit2sign :: KnownNat n => Bit -> Signed n
bit2sign 0 = 0
bit2sign 1 = 1
bit2sign _ = error "impossible bit"


encodeByte :: Signed 4 -> BitVector 8 -> (Signed 4, BitVector 10)
encodeByte dc byte =  if (wordDc == 0) || (dc == 0)
                      then if msb word == 1
                           then (dc + wordDc, lowbv ++# word)
                           else (dc - wordDc, highbv ++# lowbv ++# word8Inv)
                      else if dc == wordDc
                           then (dc + (bit2sign . msb) word - wordDc, highbv ++# (msbv' word) ++# word8Inv)
                           else (dc - (bit2sign . msb) wordInv + wordDc, lowbv ++# word)
  where ones = onesCount byte
        word = if (ones > 4) || ((ones == 4) && (lsb byte == 0))
                then lowbv ++# xnorEncode byte
                else highbv ++# xorEncode byte
        wordInv = complement word
        wordDc = (dcOffset word8) - 4
        word8 :: BitVector 8
        word8 = (snd . split) word
        word8Inv :: BitVector 8
        word8Inv = (snd . split) wordInv

decodeByte :: BitVector 10 -> BitVector 8
decodeByte word = byte''
  where (header, byte) = split word :: (BitVector 2, BitVector 8)
        invert = unpack $ msbv' header
        gate = unpack $ lsbv' header
        byte' = if invert then complement byte else byte
        byte'' = (if gate then xorDecode else xnorDecode) byte'

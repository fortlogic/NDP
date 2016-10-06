{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module NDP.TMDS (TMDS (TMDSData, TMDSControl),
                 tmdsEncoder,
                 encodeTMDS,
                 encodeByte,
                 decodeByte,
                 xorEncode,
                 xnorEncode,
                 xorDecode,
                 xnorDecode) where

import CLaSH.Prelude
import Data.Bits
import qualified Prelude as P

import NDP.Utils

data TMDS = TMDSData (Unsigned 8)
          | TMDSControl (BitVector 2)
          deriving (Show, Eq)

tmdsEncoder :: Signal TMDS -> Signal (BitVector 10)
tmdsEncoder = mealy encodeTMDS 0

encodeTMDS :: Signed 4 -> TMDS -> (Signed 4, BitVector 10)
encodeTMDS dc (TMDSData byte) = encodeByte dc (pack byte)
encodeTMDS dc (TMDSControl 0) = (dc,   $$(bLit "0010101011"))
encodeTMDS dc (TMDSControl 1) = (dc,   $$(bLit "1101010100"))
encodeTMDS dc (TMDSControl 2) = (dc-1, $$(bLit "0010101010"))
encodeTMDS dc (TMDSControl 3) = (dc+1, $$(bLit "1101010101"))

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
  where acc (bit, sum) bit' = (bit', sum + if bit == bit' then 0 else 1)

dcOffset :: (KnownNat (2 ^ n),
            KnownNat (n + 1)) =>
            BitVector (2 ^ n) -> Signed (n + 1)
dcOffset bv = (offsetOnes . foldr (+) 0 . map (resize . unpack) . bv2v) bv
  where offsetOnes ones = (2 * ones) - (int2Signed $ size# bv)

encodeByte :: Signed 4 -> BitVector 8 -> (Signed 4, BitVector 10)
encodeByte dc byte = (dc', word)
  where xored = xorEncode byte
        xnored = xnorEncode byte
        gate = transitionCount xored <= transitionCount xnored
        byte' = if gate then xored else xnored
        offset = dcOffset byte'
        invert = abs (dc - offset) < abs (dc + offset)
        offset' = if invert then 0 - offset else offset
        byte'' = if invert then complement byte' else byte'
        dc' = dc + offset'
        word = pack invert ++# pack gate ++# byte''

decodeByte :: BitVector 10 -> BitVector 8
decodeByte word = byte''
  where (header, byte) = split word :: (BitVector 2, BitVector 8)
        invert = unpack $ msb header
        gate = unpack $ lsb header
        byte' = if invert then complement byte else byte
        byte'' = (if gate then xorDecode else xnorDecode) byte'

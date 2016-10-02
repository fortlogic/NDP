{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash#-}
{-# LANGUAGE TypeOperators #-}
module TMDS (encodeByte,
             decodeByte,
             tmdsEncoder) where

import CLaSH.Prelude
import Data.Bits
import qualified Prelude as P

import Utils

xorEncode :: KnownNat n => BitVector n -> BitVector n
xorEncode = v2bv . postscanr xor 0 . bv2v

xnorEncode :: KnownNat n => BitVector n -> BitVector n
xnorEncode = v2bv . postscanr xnor 0 . bv2v
  where xnor a b = complement (xor a b)

xorDecode :: KnownNat n => BitVector n -> BitVector n
xorDecode = id -- undefined

xnorDecode :: KnownNat n => BitVector n -> BitVector n
xnorDecode = id -- undefined

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

byte :: Unsigned 8 -> BitVector 8
byte n = pack n


-- encodeByte False False byte = low ++# low ++# xnorEncode byte
-- encodeByte False True byte = low ++# high ++# xorEncode byte
-- encodeByte True False byte = high ++# low ++# complement (xnorEncode byte)
-- encodeByte True True byte = high ++# low ++# complement (xorEncode byte)

tmdsEncoder :: Signal (BitVector 8) -> Signal (BitVector 10)
tmdsEncoder = mealy encodeByte 0

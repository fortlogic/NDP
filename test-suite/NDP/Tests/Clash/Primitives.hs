{-# LANGUAGE MagicHash #-}
module Tests.Clash.Primitives where

import Clash.Prelude

topEntity :: Signal Bool -> Signal Bit -> Signal Bit -> Signal Bit
topEntity a b c = primitiveMux# (not <$> a) b c

primitiveMux# :: Signal Bool -> Signal Bit -> Signal Bit -> Signal Bit
primitiveMux# cond t f = simpleIf <$> cond <*> t <*> f
  where simpleIf True  x _ = x
        simpleIf False _ x = x

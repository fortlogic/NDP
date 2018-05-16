{-# LANGUAGE MagicHash #-}
module NDP.Tests.Clash.Primitives where

import Clash.Prelude

topEntity :: Signal domain Bool -> Signal domain Bit -> Signal domain Bit -> Signal domain Bit
topEntity a b c = primitiveMux# (not <$> a) b c

primitiveMux# :: Signal domain Bool -> Signal domain Bit -> Signal domain Bit -> Signal domain Bit
primitiveMux# cond t f = simpleIf <$> cond <*> t <*> f
  where simpleIf True  x _ = x
        simpleIf False _ x = x

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
module Primitive.ClockStrobe where

import Clash.Explicit.Prelude
import GHC.Stack

import Primitive

{-# ANN module (ndp_primitive VHDL) #-}

clockStrobe# :: ( HasCallStack
                , KnownNat stretch -- the number of fast cycles that fit in a slow one
                , KnownNat period ) -- period of the fast clock
             => Clock ('Dom fast period) gated1 -- fast clock
             -> Clock ('Dom slow (period*stretch)) gated2 -- slow clock
             -> Index stretch -- pulse offset (0 is start of cycle)
             -> Signal ('Dom fast period) Bool
clockStrobe# fast slow offset = osc
  where osc = delay fast (not <$> osc)
{-# NOINLINE clockStrobe# #-}

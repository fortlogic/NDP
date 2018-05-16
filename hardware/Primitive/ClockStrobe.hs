{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Primitive.ClockStrobe ( clockStrobe# ) where

import Clash.Explicit.Prelude
import GHC.Stack
import qualified Prelude as P

-- import Primitive

-- {-# ANN module (ndp_primitive VHDL) #-}

clockStrobe# :: ( HasCallStack
                , KnownNat stretch -- the number of fast cycles that fit in a slow one
                , KnownNat period ) -- period of the fast clock
             => Clock ('Dom fast period) gated1 -- fast clock
             -> Clock ('Dom slow (period*(stretch))) gated2 -- slow clock
             -> Index stretch -- pulse offset (0 is start of cycle)
             -> Signal ('Dom fast period) Bool
clockStrobe# fast slow offset = (fromList . (offsetList P.++) . P.cycle . toList) $ strobeCycle fast slow
  where offsetList = P.replicate ((fromInteger . toInteger) offset) False
{-# NOINLINE clockStrobe# #-}

strobeCycle :: ( KnownNat stretch
               , KnownNat period )
            => Clock ('Dom fast period) gated1
            -> Clock ('Dom slow (period*stretch)) gated2
            -> Vec stretch Bool
strobeCycle fast slow = replace 0 True lows
  where lows = replicate SNat False

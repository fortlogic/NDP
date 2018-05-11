{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module NDP.Clocking.Explicit where

import Clash.Explicit.Prelude

import Primitive.ClockStrobe



-- The fast clock speed must be an integer multiple of the slow in addition to
-- sharing a common origin. The resulting fast signal pulses true at the
-- beginning of every slow clock cycle.

clockStrobe :: ( KnownNat stretch -- the number of fast cycles that fit in a slow one
               , KnownNat period ) -- period of the fast clock
            => Clock ('Dom fast period) gated1 -- fast clock
            -> Clock ('Dom slow (period*stretch)) gated2 -- slow clock
            -> Signal ('Dom fast period) Bool
clockStrobe fastC slowC = clockStrobe# fastC slowC 0

clockStrobeD :: ( KnownNat stretch -- the number of fast cycles that fit in a slow one
                , KnownNat period ) -- period of the fast clock
             => Clock ('Dom fast period) gated1 -- fast clock
             -> Clock ('Dom slow (period*stretch)) gated2 -- slow clock
             -> Index stretch -- pulse offset (0 is start of cycle)
             -> Signal ('Dom fast period) Bool
clockStrobeD fastC slowC offset = clockStrobe# fastC slowC offset

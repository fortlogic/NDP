{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module NDP.Clocking.Explicit where

import Clash.Explicit.Prelude

import NDP.Utils.Type

-- The fast clock speed must be an integer multiple of the slow in addition to
-- sharing a common origin. The resulting fast signal pulses true at the
-- beginning of every slow clock cycle.

clockStrobe :: ( KnownDomain fast -- the fast clock domain
               , KnownDomain slow -- the slow clock domain
               , Divides (DomainPeriod fast) (DomainPeriod slow)) -- slow clock period must be a multiple of the fast one
               => Clock fast
               -> Clock slow
               -> Signal fast Bool
clockStrobe fastClock slowClock = undefined

clockStrobeD :: ( KnownDomain fast
                , KnownDomain slow
                , KnownNat stretch -- the number of fast clock cycles that can fit in a slow one
                , (DomainPeriod fast) `Divides` (DomainPeriod slow)
                , ((DomainPeriod slow) `Div` (DomainPeriod fast)) ~ stretch)
                => Clock fast
                -> Clock slow
                -> Index stretch
                -> Signal fast Bool
clockStrobeD fastClock slowClock offset = undefined

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module NDP.Primitive.ClockStrobe ( clockStrobe# ) where

import Clash.Explicit.Prelude
import GHC.Stack
import qualified Prelude as P

-- import Primitive

-- {-# ANN module (ndp_primitive VHDL) #-}

-- | Generate a synchronisation pulse in one clock domain that coincides with
-- the rising edge of another clock whose frequency is an integer multiple of
-- the first.
--
-- Note: In the event that the rising edge of the slow clock doesn't occur
-- during the first cycle of the fast clock, the offset is determined by the
-- offset variable. This offset is only used when simulating within Haskell and
-- is completely ignored by the HDL that Clash generates. Therefore this
-- parameter should not be used to offset the synchronisation pulse from the
-- rising edge of the slow clock, only to measure the *actual* phase offset of
-- the two clocks.
clockStrobe# :: ( HasCallStack
                , KnownNat period
                , KnownNat stretch
                , fastDomain ~ ('Dom fast period)
                , slowDomain ~ ('Dom slow (period*stretch)) )
             => Clock fastDomain gated1 -- ^ The fast clock.
             -> Clock slowDomain gated2 -- ^ The slow clock.
             -> Index stretch -- ^ The phase offset.
             -> Signal fastDomain Bool -- ^ The synchronisation pulse.
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

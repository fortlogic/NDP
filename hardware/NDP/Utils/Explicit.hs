{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module NDP.Utils.Explicit where

import Clash.Explicit.Prelude

pulsar :: ( KnownNat period )
        => Clock domain gated
        -> Reset domain synchronous
        -> SNat period
        -> Index period
        -> Signal domain Bool
pulsar clk rst _ off = moore clk rst step (==0) off (pure (errorX "unused"))
  where step 0 _ = maxBound
        step n _ = n - 1


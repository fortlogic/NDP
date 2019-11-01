{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module NDP.Utils.Explicit where

import Clash.Explicit.Prelude

pulsar :: ( KnownNat period
          , KnownDomain domain )
        => Clock domain
        -> Reset domain
        -> Enable domain
        -> SNat period
        -> Index period
        -> Signal domain Bool
pulsar clk rst en _ off = moore clk rst en step (==0) off (pure (errorX "unused"))
  where step 0 _ = maxBound
        step n _ = n - 1


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
module NDP.Clocking.Stretch where

import qualified Clash.Explicit.Signal as E
import qualified Clash.Explicit.Prelude as E
import Clash.Prelude

import NDP.Utils

stretchMealy# :: ( KnownNat period
                 , KnownNat stretch )
              => (acc -> a -> (acc, b))
              -> (acc -> (acc, b))
              -> acc
              -> Clock ('Dom fast period) gated1
              -> Reset ('Dom fast period) synchronous1
              -> Clock ('Dom slow (period*stretch)) gated2
              -> Index stretch
              -> Signal ('Dom slow (period*stretch)) a
              -> Signal ('Dom fast period) b
stretchMealy# inF iterF initial fastC fastR slowC offset slow = fast
  where slow' = E.unsafeSynchronizer slowC fastC slow
        sampleStrobe = (exposeClockReset pulsar) fastC fastR SNat offset
        pulse = mux sampleStrobe (Just <$> slow') (pure Nothing)
        stepF acc Nothing = iterF acc
        stepF acc (Just i) = inF acc i
        fast = E.mealy fastC fastR stepF initial pulse

stretchMoore# :: ( KnownNat period
                 , KnownNat stretch )
              => (acc -> a -> acc)
              -> (acc -> acc)
              -> (acc -> b)
              -> acc
              -> Clock ('Dom fast period) gated1
              -> Reset ('Dom fast period) synchronous1
              -> Clock ('Dom slow (period*stretch)) gated2
              -> Index stretch
              -> Signal ('Dom slow (period*stretch)) a
              -> Signal ('Dom fast period) b
stretchMoore# inF iterF outF initial fastC fastR slowC offset slow = fast
  where slow' = E.unsafeSynchronizer slowC fastC slow
        sampleStrobe = (exposeClockReset pulsar) fastC fastR SNat offset
        pulse = mux sampleStrobe (Just <$> slow') (pure Nothing)
        stepF acc Nothing = iterF acc
        stepF acc (Just i) = inF acc i
        fast = E.moore fastC fastR stepF outF initial pulse

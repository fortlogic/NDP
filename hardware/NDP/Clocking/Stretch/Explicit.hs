{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module NDP.Clocking.Stretch.Explicit ( sampleHold
                                     , sampleHoldD
                                     , stretchVec
                                     -- , stretchVecD
                                     -- , stretchVecSlice
                                     -- , stretchVecSliceD
                                     -- , stretchBitVector
                                     -- , stretchBitVectorD
                                     , stretchBitVectorSlice
                                     -- , stretchBitVectorSliceD
                                     , stretchMealy
                                     , stretchMealy'
                                     , stretchMealyD
                                     , stretchMoore
                                     , stretchMooreD ) where


import qualified Clash.Explicit.Signal as E
import qualified Clash.Explicit.Prelude as E
import Clash.Prelude

import NDP.Utils
import NDP.Clocking.Explicit

sampleHold :: ( KnownNat period
              , KnownNat stretch
              , fast ~ Dom nameF period
              , slow ~ Dom nameS (period*(1+stretch)) )
           => Clock fast gated1
           -> Reset fast synchronous1
           -> Clock slow gated2
           -> Signal slow a
           -> Signal fast a
sampleHold fastC fastR slowC slow = stretchMealy inF iterF initial fastC fastR slowC slow
  where inF _ a = (a,a)
        iterF a = (a,a)
        initial = errorX "Initial value gets ignored"

sampleHoldD :: ( KnownNat period
               , KnownNat stretch
               , fast ~ Dom nameF period
               , slow ~ Dom nameS (period*(1+stretch)) )
            => Clock fast gated1
            -> Reset fast synchronous1
            -> Clock slow gated2
            -> a
            -> Index (1+stretch)
            -> Signal slow a
            -> Signal fast a
sampleHoldD fastC fastR slowC initial offset slow = stretchMealyD inF iterF initial fastC fastR slowC offset slow
  where inF _ a = (a,a)
        iterF a = (a,a)

stretchVec :: ( KnownNat period
              , KnownNat stretch
              , fast ~ Dom nameF period
              , slow ~ Dom nameS (period*(1+stretch)) )
           => Clock fast gated1
           -> Reset fast synchronous1
           -> Clock slow gated2
           -> Signal slow (Vec (1+stretch) a)
           -> Signal fast a
stretchVec fastC fastR slowC slow = stretchMealy inF iterF initial fastC fastR slowC slow
  where inF _ v = iterF v
        iterF v = (v <<+ (errorX "bubble"), head v)
        initial = errorX "Initial value gets ignored"

-- stretchVecD


-- stretchVecSlice :: ( KnownNat period
--                    , KnownNat stretch
--                    , KnownNat sliceSize
--                    , fast ~ Dom nameF period
--                    , slow ~ Dom nameS (period*(1+stretch)) )
--                 => Clock fast gated1
--                 -> Reset fast synchronous1
--                 -> Clock slow gated2
--                 -> Signal slow (Vec ((1+sliceSize)*(1+stretch)) a)
--                 -> Signal fast (Vec (1+sliceSize) a)

-- stretchVecSliceD

-- stretchBitVector :: ( KnownNat period
--                     , KnownNat stretch
--                     , fast ~ Dom nameF period
--                     , slow ~ Dom nameS (period*(1+stretch)) )
--                  => Clock fast gated1
--                  -> Reset fast synchronous1
--                  -> Clock slow gated2
--                  -> Signal slow (BitVector (1+stretch))
--                  -> Signal fast Bit

-- stretchBitVectorD

stretchBitVectorSlice :: ( KnownNat period
                         , KnownNat stretch
                         , KnownNat sliceSize
                         , fast ~ Dom nameF period
                         , slow ~ Dom nameS (period*(1+stretch)) )
                      => Clock fast gated1
                      -> Reset fast synchronous1
                      -> Clock slow gated2
                      -> Signal slow (BitVector ((1+sliceSize)*(1+stretch)))
                      -> Signal fast (BitVector (1+sliceSize))
stretchBitVectorSlice fastC fastR slowC slow = stretchVec fastC fastR slowC slowVec
  where slowVec = partitionbv <$> slow <*> pure SNat

-- stretchBitVectorSliceD

--
-- Mealy and Moore machine stretchers
--
-- These are the primitives upon which all other stretchers are based.

stretchMealy :: ( KnownNat period
                , KnownNat stretch
                , fast ~ Dom nameF period
                , slow ~ Dom nameS (period*(1+stretch)) )
             => (acc -> a -> (acc, b))
             -> (acc -> (acc, b))
             -> acc
             -> Clock fast gated1
             -> Reset fast synchronous1
             -> Clock slow gated2
             -> Signal slow a
             -> Signal fast b
stretchMealy inF iterF initial fastC fastR slowC slow = stretchMealyD inF iterF initial fastC fastR slowC 0 slow

stretchMealy' :: ( KnownNat period
                 , KnownNat stretch
                 , fast ~ Dom nameF period
                 , slow ~ Dom nameS (period*(1+stretch)) )
              => (a -> (acc, b))
              -> (acc -> (acc, b))
              -> Clock fast gated1
              -> Clock slow gated2
              -> Signal slow a
              -> Signal fast b
stretchMealy' inF iterF fastC slowC slow = undefined
  where slow' = E.unsafeSynchronizer slowC fastC slow

stretchMealyD :: ( KnownNat period
                 , KnownNat stretch
                 , fast ~ Dom nameF period
                 , slow ~ Dom nameS (period*(1+stretch)) )
              => (acc -> a -> (acc, b))
              -> (acc -> (acc, b))
              -> acc
              -> Clock fast gated1
              -> Reset fast synchronous1
              -> Clock slow gated2
              -> Index (1+stretch)
              -> Signal slow a
              -> Signal fast b
stretchMealyD inF iterF initial fastC fastR slowC offset slow = fast
  where slow' = E.unsafeSynchronizer slowC fastC slow
        sampleStrobe = clockStrobeD fastC slowC offset
        pulse = mux sampleStrobe (Just <$> slow') (pure Nothing)
        stepF acc Nothing = iterF acc
        stepF acc (Just i) = inF acc i
        fast = E.mealy fastC fastR stepF initial pulse

stretchMoore :: ( KnownNat period
                , KnownNat stretch
                , fast ~ Dom nameF period
                , slow ~ Dom nameS (period*(1+stretch)) )
             => (acc -> a -> acc)
             -> (acc -> acc)
             -> (acc -> b)
             -> acc
             -> Clock fast gated1
             -> Reset fast synchronous1
             -> Clock slow gated2
             -> Signal slow a
             -> Signal fast b
stretchMoore inF iterF outF initial fastC fastR slowC slow = stretchMooreD inF iterF outF initial fastC fastR slowC 0 slow

stretchMooreD :: ( KnownNat period
                , KnownNat stretch
                , fast ~ Dom nameF period
                , slow ~ Dom nameS (period*(1+stretch)) )
              => (acc -> a -> acc)
              -> (acc -> acc)
              -> (acc -> b)
              -> acc
              -> Clock fast gated1
              -> Reset fast synchronous1
              -> Clock slow  gated2
              -> Index stretch
              -> Signal slow a
              -> Signal fast b
stretchMooreD inF iterF outF initial fastC fastR slowC offset slow = fast
  where slow' = E.unsafeSynchronizer slowC fastC slow
        sampleStrobe = (exposeClockReset pulsar) fastC fastR SNat offset
        pulse = mux sampleStrobe (Just <$> slow') (pure Nothing)
        stepF acc Nothing = iterF acc
        stepF acc (Just i) = inF acc i
        fast = E.moore fastC fastR stepF outF initial pulse

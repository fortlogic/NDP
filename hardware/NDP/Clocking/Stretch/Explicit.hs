{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
                                     , stretchMealyD
                                     , stretchMoore
                                     , stretchMooreD ) where


import qualified Clash.Explicit.Signal as E
import qualified Clash.Explicit.Prelude as E
import Clash.Prelude

import NDP.Utils
import NDP.Utils.Type
import NDP.Clocking.Explicit

sampleHold :: ( KnownDomain fast
              , KnownDomain slow
              , KnownNat ((DomainPeriod slow) `Div` (DomainPeriod fast))
              , (DomainPeriod fast) `Divides` (DomainPeriod slow)
              , NFDataX a)
           => Clock fast
           -> Reset fast
           -> Enable fast
           -> Clock slow
           -> Signal slow a
           -> Signal fast a
sampleHold fastC fastR fastEn slowC slow = machine
  where machine = stretchMealy fastC fastR fastEn slowC inF iterF initial slow
        inF _ a = (a,a)
        iterF a = (a,a)
        -- previous states aren't used to calculate the next state, this is never used
        initial = errorX "ignored"

sampleHoldD :: ( KnownDomain fast
               , KnownDomain slow
               , KnownNat stretch
               , (DomainPeriod fast) `Divides` (DomainPeriod slow)
               , stretch ~ ((DomainPeriod slow) `Div` (DomainPeriod fast))
               , NFDataX a )
            => Clock fast
            -> Reset fast
            -> Enable fast
            -> Clock slow
            -> Index stretch
            -> a
            -> Signal slow a
            -> Signal fast a
sampleHoldD fastC fastR fastEn slowC offset initial slow = machine
  where machine = stretchMealyD fastC fastR fastEn slowC offset inF iterF initial slow
        inF _ a = (a,a)
        iterF a = (a,a)

stretchVec :: (  KnownDomain fast
               , KnownDomain slow
               , KnownNat stretch
               , stretch ~ ((DomainPeriod slow) `Div` (DomainPeriod fast))
               , (DomainPeriod fast) `Divides` (DomainPeriod slow)
               , NFDataX a )
           => Clock fast
           -> Reset fast
           -> Enable fast
           -> Clock slow
           -> Signal slow (Vec stretch a)
           -> Signal fast a
stretchVec fastC fastR fastEn slowC slow = fast
  where fast = stretchMealy fastC fastR fastEn slowC inF iterF initial slow
        inF _ = iterF
        iterF = (\ (a, b) -> (a, head b)) . ((flip shiftInAtN) (bubble :> Nil))
        -- these are never exposed to the outside
        bubble = errorX "bubble"
        initial = errorX "Initial value gets ignored"


-- stretchVecD



-- stretchVecSlice :: ( KnownNat period
--                    , KnownNat stretch
--                    , KnownNat sliceSize
--                    , 1 <= stretch
--                    , 1 <= sliceSize
--                    , fast ~ 'Dom nameF period
--                    , slow ~ 'Dom nameS (period*stretch) )
--                 => Clock fast gated1
--                 -> Reset fast synchronous1
--                 -> Clock slow gated2
--                 -> Signal slow (Vec (sliceSize*stretch) a)
--                 -> Signal fast (Vec sliceSize a)

-- stretchVecSliceD

-- stretchBitVector :: ( KnownNat period
--                     , KnownNat stretch
--                     , 1 <= stretch
--                     , fast ~ 'Dom nameF period
--                     , slow ~ 'Dom nameS (period*stretch) )
--                  => Clock fast gated1
--                  -> Reset fast synchronous1
--                  -> Clock slow gated2
--                  -> Signal slow (BitVector stretch)
--                  -> Signal fast Bit

-- stretchBitVectorD

stretchBitVectorSlice :: ( KnownDomain fast
                         , KnownDomain slow
                         , KnownNat stretch
                         , KnownNat sliceSize
                         , stretch ~ ((DomainPeriod slow) `Div` (DomainPeriod fast))
                         , (DomainPeriod fast) `Divides` (DomainPeriod slow) )
                      => Clock fast
                      -> Reset fast
                      -> Enable fast
                      -> Clock slow
                      -> Signal slow (BitVector (sliceSize `Mult` stretch))
                      -> Signal fast (BitVector sliceSize)
stretchBitVectorSlice fastC fastR fastEn slowC slow = stretchVec fastC fastR fastEn slowC slowVec
  where slowVec = partitionbv <$> slow <*> pure SNat

-- stretchBitVectorSliceD

--
-- Mealy and Moore machine stretchers
--
-- These are the primitives upon which all other stretchers are based.

stretchMealy :: ( KnownDomain fast
                , KnownDomain slow
                , KnownNat ((DomainPeriod slow) `Div` (DomainPeriod fast))
                , (DomainPeriod fast) `Divides` (DomainPeriod slow)
                , NFDataX acc)
             => Clock fast
             -> Reset fast
             -> Enable fast
             -> Clock slow
             -> (acc -> a -> (acc, b))
             -> (acc -> (acc, b))
             -> acc
             -> Signal slow a
             -> Signal fast b
stretchMealy fastC fastR fastEn slowC inF iterF initial slow = fast
  where fast = stretchMealyD fastC fastR fastEn slowC 0 inF iterF initial slow

stretchMealyD :: ( KnownDomain fast
                 , KnownDomain slow
                 , KnownNat stretch
                 , (DomainPeriod fast) `Divides` (DomainPeriod slow)
                 , stretch ~ ((DomainPeriod slow) `Div` (DomainPeriod fast))
                 , NFDataX acc)
              => Clock fast
              -> Reset fast
              -> Enable fast
              -> Clock slow
              -> Index stretch
              -> (acc -> a -> (acc, b))
              -> (acc -> (acc, b))
              -> acc
              -> Signal slow a
              -> Signal fast b
stretchMealyD fastC fastR fastEn slowC offset inF iterF initial slow = fast
  where slow' = E.unsafeSynchronizer slowC fastC slow
        sampleStrobe = clockStrobeD fastC slowC offset
        pulse = mux sampleStrobe (Just <$> slow') (pure Nothing)
        stepF acc Nothing = iterF acc
        stepF acc (Just i) = inF acc i
        fast = E.mealy fastC fastR fastEn stepF initial pulse

stretchMoore :: ( KnownDomain fast
                , KnownDomain slow
                , KnownNat ((DomainPeriod slow) `Div` (DomainPeriod fast))
                , (DomainPeriod fast) `Divides` (DomainPeriod slow)
                , NFDataX acc)
             => Clock fast
             -> Reset fast
             -> Enable fast
             -> Clock slow
             -> (acc -> a -> acc)
             -> (acc -> acc)
             -> (acc -> b)
             -> acc
             -> Signal slow a
             -> Signal fast b
stretchMoore fastC fastR fastEn slowC inF iterF outF initial slow = fast
  where fast = stretchMooreD fastC fastR fastEn slowC 0 inF iterF outF initial slow

stretchMooreD :: ( KnownDomain fast
                 , KnownDomain slow
                 , KnownNat stretch
                 , (DomainPeriod fast) `Divides` (DomainPeriod slow)
                 , stretch ~ ((DomainPeriod slow) `Div` (DomainPeriod fast))
                 , NFDataX acc)
              => Clock fast
              -> Reset fast
              -> Enable fast
              -> Clock slow
              -> Index stretch
              -> (acc -> a -> acc)
              -> (acc -> acc)
              -> (acc -> b)
              -> acc
              -> Signal slow a
              -> Signal fast b
stretchMooreD fastC fastR fastEn slowC offset inF iterF outF initial slow = fast
  where slow' = E.unsafeSynchronizer slowC fastC slow
        sampleStrobe = (exposeClockResetEnable pulsar) fastC fastR fastEn SNat offset
        pulse = mux sampleStrobe (Just <$> slow') (pure Nothing)
        stepF acc Nothing = iterF acc
        stepF acc (Just i) = inF acc i
        fast = E.moore fastC fastR fastEn stepF outF initial pulse

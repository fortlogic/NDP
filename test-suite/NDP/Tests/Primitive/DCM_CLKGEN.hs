{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module NDP.Tests.Primitive.DCM_CLKGEN ( properties ) where

import Clash.Prelude
-- import Data.Maybe
-- import Data.Proxy
-- import GHC.TypeLits
import Test.Hspec
-- import Test.QuickCheck

-- grabNat :: Integral a => Positive a -> SomeNat
-- grabNat = fromJust . someNatVal . toInteger . getPositive

-- How do I write this?
-- someToSNat :: SomeNat -> SNat n
-- someToSNat (SomeNat (p :: Proxy n)) = snatProxy p -- (Proxy :: Proxy n)

properties :: String -> Spec
properties name = describe name $ do
  it "can generate test clocks" $ do
    pendingWith "eh"
  it "does stuff" $ do
    pendingWith "huh"

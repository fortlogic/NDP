{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module PopCountTest.Main where

import Data.Kind
import Data.Singletons
import Data.Word
import Clash.Prelude

{-# ANN topEntity Synthesize
          { t_name = "PopCountTest"
          , t_inputs = [ PortName "wing_a"
                       , PortName "wing_b" ]
          , t_output = PortName "wing_c"
          } #-}

topEntity :: Word16 -> Word16 -> BitVector 16
topEntity a b = (pack popa) ++# (pack popb)
  where popa = ((extend . popcount . w2bv) a) :: Index 256
        popb = ((extend . popcount . w2bv) b) :: Index 256

w2bv :: Word16 -> BitVector 16
w2bv = pack

bv2t :: KnownNat n => BitVector (2 ^ n) -> RTree n Bit
bv2t = v2t . unpack

data Width2Index (f :: TyFun Nat Type) :: Type
type instance Apply Width2Index n = Index ((2^n)+1)

popcount :: KnownNat n => BitVector (2 ^ n) -> Index ((2^n)+1)
popcount bv = tdfold (Proxy @Width2Index) fromIntegral (\_ a b -> add a b) rt
  where rt = bv2t bv


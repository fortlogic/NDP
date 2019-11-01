{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module NDP.Utils (int2Signed,
                  bvXForm,
                  swap,
                  pulsar,
                  highbv,
                  lowbv,
                  bvcons,
                  bvsnoc,
                  msbv,
                  lsbv,
                  msbv',
                  lsbv',
                  partitionbv,
                  partitionbv'
                  -- shuffleInside,
                  -- shuffleOutside
                  ) where

import Clash.Prelude
import Clash.Sized.Internal.BitVector (split#)

import qualified NDP.Utils.Explicit as E
import NDP.Utils.Type

int2Signed :: KnownNat n => Int -> Signed n
int2Signed = fromInteger . toInteger

bvXForm :: KnownNat n =>
           (Vec n Bit -> Vec n Bit) -> (BitVector n -> BitVector n)
bvXForm f = v2bv . f . bv2v

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

pulsar :: ( KnownNat period
          , HiddenClockResetEnable domain )
        => SNat period
        -> Index period
        -> Signal domain Bool
pulsar = hideClockResetEnable E.pulsar

highbv :: BitVector 1
highbv = $$(bLit "1")

lowbv :: BitVector 1
lowbv = $$(bLit "0")

bvcons :: KnownNat n => Bit -> BitVector n -> BitVector (1+n)
bvcons b bs = (pack b) ++# bs

bvsnoc :: KnownNat n => BitVector n -> Bit -> BitVector (1+n)
bvsnoc = flip bvcons

msbv :: (BitPack a, KnownNat (BitSize a), KnownNat n, KnownNat m, (BitSize a) ~ (n+m) ) => SNat n -> a -> BitVector n
msbv _ a = prefix
  where (prefix, _) = split a

msbv' :: (BitPack a, KnownNat (BitSize a), KnownNat m, (BitSize a) ~ (1+m)) => a -> BitVector 1
msbv' = msbv SNat

lsbv :: (BitPack a, KnownNat (BitSize a), KnownNat n, KnownNat m, (BitSize a) ~ (m+n) ) => SNat n -> a -> BitVector n
lsbv _ a = suffix
  where (_, suffix) = split a

lsbv' :: (BitPack a, KnownNat (BitSize a), KnownNat m, (BitSize a) ~ (m+1)) => a -> BitVector 1
lsbv' = lsbv SNat

partitionbv :: (KnownNat n, KnownNat m) => BitVector (n `Mult` m) -> SNat n -> Vec n (BitVector m)
partitionbv bv n = partitionbv' bv (toUNat n)

partitionbv' :: (KnownNat n, KnownNat m) => BitVector (n `Mult` m) -> UNat n -> Vec n (BitVector m)
partitionbv' _  UZero     = Nil
partitionbv' bv (USucc n) = prefix :> (partitionbv' suffix n)
  where (prefix, suffix) = split# bv

-- -- "123456" -> "162534" -- ("123456" -> "16" "25" "34" -> "162534")
-- shuffleOutside :: (KnownNat n,
--                    KnownNat (n + n),
--                    KnownNat (2 * n)) => Vec n a -> Vec n a
-- shuffleOutside vs = takeI pairs
--   where pairs = castWith (sym dist2VecPrf) $ merge vs (reverse vs)


-- -- "123456" -> "342516" -- ("123456" -> "34" "25" "16" -> "342516")
-- shuffleInside :: (KnownNat n,
--                   KnownNat (n + n),
--                   KnownNat (2 * n)) => Vec n a -> Vec n a
-- shuffleInside vs = dropI pairs
--   where pairs = castWith (sym dist2VecPrf) $ merge vs (reverse vs)


-- Strange type level stuff ;)

-- dist2VecPrf :: (KnownNat (n + n),
--                 KnownNat (2 * n)) => (Vec (n + n) a :~: Vec (2 * n) a)
-- dist2VecPrf = prf
--   where (Just prf) = vecEqPrf <$> distribute2Prf


-- distribute2Prf :: (KnownNat (n + n),
--                    KnownNat (2 * n)) => Maybe ((n + n) :~: (2 * n))
-- distribute2Prf = sameNat Proxy Proxy

-- vecEqPrf :: (n :~: n') -> (Vec n a :~: Vec n' a)
-- vecEqPrf prf = apply (apply Refl prf) Refl

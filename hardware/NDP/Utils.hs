{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module NDP.Utils (int2Signed,
                  bvXForm,
                  swap
                  -- shuffleInside,
                  -- shuffleOutside
                  ) where

import Clash.Prelude
-- import Data.Proxy
-- import Data.Type.Equality
-- import qualified Prelude as P

int2Signed :: KnownNat n => Int -> Signed n
int2Signed = fromInteger . toInteger

bvXForm :: KnownNat n =>
           (Vec n Bit -> Vec n Bit) -> (BitVector n -> BitVector n)
bvXForm f = v2bv . f . bv2v

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

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

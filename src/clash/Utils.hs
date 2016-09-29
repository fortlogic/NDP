module Utils (int2Signed) where

import CLaSH.Prelude

int2Signed :: KnownNat n => Int -> Signed n
int2Signed = fromInteger . toInteger

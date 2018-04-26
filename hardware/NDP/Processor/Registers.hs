{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module NDP.Processor.Registers where

import Clash.Prelude
import Data.Singletons.Prelude



-- register file exposes `2^fi` active 'frames' from a set of `2^pa` pages.
registerFile :: ( KnownNat rc -- (per frame) register count
                , KnownNat fc -- active frame count
                , KnownNat pc -- total page count
                ) =>
                Signal (Vec fc (Index pc)) -> -- active frame selectors
                Signal (Index (rc * fc)) -> -- read 1
                Signal (Index (rc * fc)) -> -- read 2
                Signal (Maybe (Index (rc * fc), w)) -> -- maybe a write
                Signal (w, w) -- output reads
registerFile _ _ _ _ = undefined

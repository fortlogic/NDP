{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module NDP.Processor.Registers where

import Clash.Prelude
import qualified Data.Singletons.Prelude as S



-- register file exposes `2^fi` active 'frames' from a set of `2^pa` pages.
registerFile :: ( KnownNat rc -- (per frame) register count
                , KnownNat fc -- active frame count
                , KnownNat pc -- total page count
                ) =>
                Signal domain (Vec fc (Index pc)) -> -- active frame selectors
                Signal domain (Index (rc * fc)) -> -- read 1
                Signal domain (Index (rc * fc)) -> -- read 2
                Signal domain (Maybe (Index (rc * fc), w)) -> -- maybe a write
                Signal domain (w, w) -- output reads
registerFile _ _ _ _ = undefined

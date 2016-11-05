{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module TopLevel.MinimalDVIDPlusTMDS.Main where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import CLaSH.Prelude.Synchronizer

import NDP.Clocking
import NDP.Video.TMDS

{-# ANN topEntity
  (defTop {
     t_name = "tmds_encoder",
     t_inputs = ["blank_en", "ctl_in", "px_in"],
     t_outputs = ["tmds_out"]
   }) #-}

topEntity :: SignalPx Bit ->           -- ctl enable
             SignalPx (BitVector 2) -> -- control
             SignalPx (Unsigned 8) ->  -- pixel
             SignalPx (BitVector 10)   -- tmds word
topEntity cEn ctl px = fakeTMDS <$> cEn <*> ctl <*> msb3S
  where msb3S = (unpack . slice d7 d5) <$> px


fakeTMDS :: Bit -> (BitVector 2) -> Unsigned 3 -> (BitVector 10)
fakeTMDS 1 0 _ = $$(bLit "1101010100")
fakeTMDS 1 1 _ = $$(bLit "0010101011")
fakeTMDS 1 2 _ = $$(bLit "0101010100")
fakeTMDS 1 3 _ = $$(bLit "1010101011")
fakeTMDS 0 _ 0 = $$(bLit "0111110000")
fakeTMDS 0 _ 1 = $$(bLit "0001001111")
fakeTMDS 0 _ 2 = $$(bLit "0111001100")
fakeTMDS 0 _ 3 = $$(bLit "0010001111")
fakeTMDS 0 _ 4 = $$(bLit "0000101111")
fakeTMDS 0 _ 5 = $$(bLit "1000111001")
fakeTMDS 0 _ 6 = $$(bLit "1000011011")
fakeTMDS 0 _ 7 = $$(bLit "1011110000")

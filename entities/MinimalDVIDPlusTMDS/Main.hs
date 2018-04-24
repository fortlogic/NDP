{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module MinimalDVIDPlusTMDS.Main where

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

topEntity :: SignalPx Bool ->          -- ctl enable
             SignalPx (BitVector 2) -> -- control
             SignalPx (Unsigned 8) ->  -- pixel
             SignalPx (BitVector 10)   -- tmds word
topEntity cEn ctl px = registerPx 0 $ tmdsEncoder tmdsIn
  where tmdsIn = bitsToTMDS <$> registerPx True cEn <*> registerPx 0 ctl <*> registerPx 0 px


bitsToTMDS :: Bool ->
              BitVector 2 ->
              Unsigned 8 -> TMDS
bitsToTMDS False _ px  = TMDSData px
bitsToTMDS True ctl _ = TMDSControl ctl

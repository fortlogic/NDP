{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module MinimalDVIDPlusTMDS.Main where

import Clash.Prelude
import Clash.Explicit.Synchronizer

import NDP.Clocking.Domains
import NDP.Video.TMDS

{-# ANN topEntity Synthesize
          { t_name = "tmds_encoder"
          , t_inputs = [ PortName "HDMI4000"
                       , PortName "HDMI4000_rstn"
                       , PortName "blank_en"
                       , PortName "ctl_in"
                       , PortName "px_in" ]
          , t_output = PortName "tmds_out"
          } #-}

topEntity :: Clock PixelD Source ->
             Reset PixelD Asynchronous ->
             Signal PixelD Bool ->
             Signal PixelD (BitVector 2) ->
             Signal PixelD (Unsigned 8) ->
             Signal PixelD (BitVector 10)
topEntity = exposeClockReset hdmiEncoder

hdmiEncoder :: (HiddenClockReset domain gated synchronous) =>
               Signal domain Bool ->          -- ctl enable
               Signal domain (BitVector 2) -> -- control
               Signal domain (Unsigned 8) ->  -- pixel
               Signal domain (BitVector 10)   -- tmds word
hdmiEncoder cEn ctl px = register 0 $ tmdsEncoder tmdsIn
  where tmdsIn = bitsToTMDS <$> register True cEn <*> register 0 ctl <*> register 0 px


bitsToTMDS :: Bool ->
              BitVector 2 ->
              Unsigned 8 -> TMDS
bitsToTMDS False _ px  = TMDSData px
bitsToTMDS True ctl _ = TMDSControl ctl

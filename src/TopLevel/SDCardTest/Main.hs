{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module TopLevel.PureClashTest.Main where

import CLaSH.Prelude
import CLaSH.Signal.Explicit

import NDP.Clocking
import NDP.IO.SDCard

--topEntity = sdCard# rawClk (SNat :: SNat 5000) (SNat :: SNat 40) (SNat :: SNat 2500) (SNat :: SNat 512)

{-# ANN topEntity
  (defTop {
     t_name = "SDCardTest",
     t_inputs = ["sd_miso"],
     t_outputs = ["led_hdmi_green",
                  "led_hdmi_red",
                  "led_sd_green",
                  "led_sd_red",
                  "led_usb_red",
                  "sd_cs",
                  "sd_mosi",
                  "sd_clk"],
     t_extraIn = [("clk_vec_in", 1),
                  ("button", 1)],
     t_clocks = [ ClockSource {
       c_name = "dumb_clock",
       c_inp = [("raw_clk", "clk_vec_in(0)")],
       c_outp = [("main_clk", show (rawClk :: SClock RawClk))],
       c_reset = Just ("reset", "button(0)"),
       c_lock = "stable",
       c_sync = False
     } ]
   }) #-}
topEntity :: SignalRaw ( Bit -- SD: miso
                    ) -> SignalRaw ( Bit, -- LED: HDMI green
                                     Bit, -- LED: HDMI red
                                     Bit, -- LED: SD green
                                     Bit, -- LED: SD red
                                     Bit, -- LED: USB red
                                     Bit, -- SD: chip select
                                     Bit, -- SD: mosi
                                     Bit) -- SD: clock
topEntity miso = register' rawClk (low, low, low, high, high, 0, 0, 0) (bundle outputs)
  where outputs = (pure low, -- lights
                   pure low,
                   ready,
                   err,
                   pure low,
                   sdCs, sdMosi, sdClk) -- sd card
        (sdCs, sdMosi, sdClk, ready, err) = unbundle $ tiedOffSD miso

tiedOffSD :: SignalRaw Bit -> SignalRaw (Bit, Bit, Bit, Bit, Bit) -- cs, mosy, sdclk, ready, err
tiedOffSD miso = bundle (cs, spiClk, mosi, ready, isErr)
  where reset = pure 0
        readReq = pure 0
        writeReq = pure 0
        k = pure 0
        addr = pure 0
        d = pure 0
        h = pure 0
        (d', busy, h', err, cs, spiClk, mosi) = unbundle sdOut
        sdOut = timedSD reset readReq writeReq k addr d h miso
        timedSD = sdController# rawClk SNat (SNat :: SNat 40) (SNat :: SNat 2500) (SNat :: SNat 512)
        ready = flipBit <$> busy
        isErr = (\ e -> if e == 0 then 0 else 1) <$> err

flipBit :: Bit -> Bit
flipBit 0 = 1
flipBit 1 = 0

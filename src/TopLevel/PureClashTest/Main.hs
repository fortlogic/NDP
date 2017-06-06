{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module TopLevel.PureClashTest.Main where

import CLaSH.Prelude
import CLaSH.Signal.Explicit

{-# ANN topEntity
  (defTop {
     t_name = "PureClashTest",
     t_inputs = [],
     t_outputs = ["led_hdmi_green",
                  "led_hdmi_red",
                  "led_sd_green",
                  "led_sd_red",
                  "led_usb_red"],
     t_extraIn = [("clk_vec_in", 1),
                  ("button", 1)],
     t_clocks = [ ClockSource {
       c_name = "dumb_clock",
       c_inp = [("raw_clk", "clk_vec_in(0)")],
       c_outp = [("main_clk", show (sclock :: SClock SystemClock))],
       c_reset = Just ("reset", "not button(0)"),
       c_lock = "stable",
       c_sync = False
     } ]
   }) #-}
topEntity :: Signal (Bit, Bit, Bit, Bit, Bit)
topEntity = register (high, low, high, low, high) (signal lights)
  where lights = (low, high, low, high, low)

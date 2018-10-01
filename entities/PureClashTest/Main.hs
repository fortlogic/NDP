{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module PureClashTest.Main where

import Clash.Prelude

import NDP.Clocking.Domains

{-# ANN topEntity
  (Synthesize {
     t_name = "PureClashTest",
     t_inputs = [ PortName "clk_in"
                , PortName "button" ],
     t_output = PortProduct
                   ""
                   [ PortName "led_hdmi_green"
                   , PortName "led_hdmi_red"
                   , PortName "led_sd_green"
                   , PortName "led_sd_red"
                   , PortName "led_usb_red" ]
   }) #-}
topEntity :: Clock OutsideD Source
          -> Reset OutsideD Asynchronous
          -> Signal OutsideD (Bit, Bit, Bit, Bit, Bit)
topEntity clk rst = inverter clk rst (low, high, low, high, low)
  -- exposeClockReset out
  -- where out = register (high, low, high, low, high) (pure lights)
  --      lights = (low, high, low, high, low)


{-# ANN inverter
  (Synthesize
    { t_name = "reset_inverter"
    , t_inputs = [ PortName "values" ]
    , t_output = PortName "reg_out"
  }) #-}
inverter :: Clock OutsideD Source
         -> Reset OutsideD Asynchronous
         -> (Bit, Bit, Bit, Bit, Bit)
         -> Signal OutsideD (Bit, Bit, Bit, Bit, Bit)
inverter clk rst a = (exposeClockReset register) clk rst initial (pure a)
  where initial = (unpack . complement . pack) a
{-# NOINLINE inverter #-}

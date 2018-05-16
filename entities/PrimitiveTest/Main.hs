{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module PrimitiveTest.Main where

import Clash.Annotations.TopEntity
import Clash.Prelude

import Primitive.ClockStrobe
import NDP.Clocking.Domains

{-# ANN strobeTest (Synthesize "clockStrobe" [PortName "fast_clk", PortName "slow_clk"] (PortName "strobe_out")) #-}
{-# ANN strobeTest (TestBench 'strobeTestBench) #-}
strobeTest :: Clock OutsideD Source -> Clock (StretchDomain OutsideD 5) Source -> Signal OutsideD Bool
strobeTest fast slow = clockStrobe# fast slow 3

strobeTestBench :: Signal OutsideD Bool
strobeTestBench = undefined
  where expectOutput = undefined
        fastClk = tbClockGen @OutsideD
        slowClk = undefined

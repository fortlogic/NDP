module PrimitiveTest.Main where

import Clash.Annotations.TopEntity

import Primitive.ClockStrobe
import NDP.Clocking.Domains

{-# ANN strobeTest (Synthesize "strobeTest" [PortName "fast_clk", PortName "slow_clk"] (PortName "strobe_out")) #-}
strobeTest :: Clock OutsideD Source -> Clock (StretchDomain OutsideD 4) Source -> Signal OutsideD Bool
strobeTest fast slow = pure False

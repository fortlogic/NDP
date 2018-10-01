{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module PrimitiveTest.Main where

import Clash.Annotations.TopEntity
import Clash.Explicit.Signal
import Clash.Explicit.Testbench
import Clash.Prelude

import NDP.Clocking.Domains
import NDP.Primitive.ClockStrobe

{-# ANN strobeTest (Synthesize "clockStrobe" [PortName "fast_clk", PortName "slow_clk"] (PortName "strobe_out")) #-}
{-# ANN strobeTest (TestBench 'strobeTestBench) #-}
strobeTest :: Clock OutsideD Source -> Clock (StretchDomain OutsideD 5) Source -> Signal OutsideD Bool
strobeTest fast slow = clockStrobe# fast slow 3

strobeTestBench :: Signal OutsideD Bool
strobeTestBench = done
  where expectOutput = outputVerifier fastClk fastRst $(listToVecTH [ False :: Bool, False, False,
                                                                      True, False, False, False, False,
                                                                      True, False, False, False, False,
                                                                      True, False, False, False, False,
                                                                      True, False, False, False, False ])
        done = expectOutput (strobeTest fastClk slowClk)
        fastClk = tbClockGen @OutsideD (not <$> done)
        slowClk = tbClockGen @(StretchDomain OutsideD 5) (unsafeSynchronizer fastClk slowClk (not <$> done))
        fastRst = asyncResetGen @OutsideD

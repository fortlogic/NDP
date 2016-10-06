{-# LANGUAGE DataKinds #-}
module NDP.Clocking (PixelClock,
                     TMDSClokc) where

import CLaSH.Prelude.Explicit

-- TMDS clock is the x5 DDR clock
type PixelClock = Clk "HDMI" 1000
type TMDSClock  = Clk "HDMI" 200

pixelClk :: SClock PixelClock
pixelClk = sclock

tmdsClk :: SClock TMDSClock
tmdsClk = sclock

{-# LANGUAGE DataKinds #-}
module NDP.Clocking (PixelClk,
                     PixelDDRClk,
                     pxClk,
                     pxDDRClk,
                     SignalPx,
                     SignalPxDDR) where

import CLaSH.Prelude.Explicit

-- TMDS clock is the x5 DDR clock
type PixelClk = Clk "HDMI" 1000
type PixelDDRClk  = Clk "HDMI" 200

pxClk :: SClock PixelClk
pxClk = sclock

pxDDRClk :: SClock PixelDDRClk
pxDDRClk = sclock

type SignalPx a = Signal' PixelClk a
type SignalPxDDR a = Signal' PixelDDRClk a

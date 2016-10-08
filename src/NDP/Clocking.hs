{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
module NDP.Clocking (PixelClk,
                     Pixelx5Clk,
                     pxClk,
                     px5Clk,
                     SignalPx,
                     SignalPx5,
                     clockStrobe,
                     pxTo5x) where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import CLaSH.Sized.Internal.BitVector (shiftR#)

-- TMDS clock is the x5 DDR clock
type PixelClk = Clk "HDMI" 1000
type Pixelx5Clk  = Clk "HDMI" 200

pxClk :: SClock PixelClk
pxClk = sclock

px5Clk :: SClock Pixelx5Clk
px5Clk = sclock

type SignalPx a = Signal' PixelClk a
type SignalPx5 a = Signal' Pixelx5Clk a

-- The fast clock speed must be an integer multiple of the slow in addition to
-- sharing a common origin. The resulting fast signal pulses true at the
-- beginning of every slow clock cycle.
clockStrobe :: SClock slowClk -> SClock fastClk -> Signal' fastClk Bool
clockStrobe slowClk fastClk  = xor <$> fastOSC <*> fastOSC'
  where slowOSC = register' slowClk False (not <$> slowOSC)
        fastOSC = unsafeSynchronizer slowClk fastClk slowOSC
        fastOSC' = register' fastClk  False fastOSC

-- Latches onto the input word at the start of every pixel clock cycle.
pxTo5x :: SignalPx (BitVector 10) -> SignalPx5 (BitVector 2)
pxTo5x tmdsIn = tail
  where tmds = unsafeSynchronizer pxClk px5Clk tmdsIn
        strobe = clockStrobe pxClk px5Clk
        (body, tail) = unbundle' px5Clk $ split <$> mux strobe tmds body'
        body' = register' px5Clk 0 $ (++#) <$> tail <*> (body :: SignalPx5 (BitVector 8))

-- simulate pxTo5x [$$(bLit "0101010101"), $$(bLit "0111100001"), $$(bLit "1000011110")]

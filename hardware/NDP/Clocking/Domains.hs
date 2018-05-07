{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
module NDP.Clocking.Domains where

import Clash.Prelude

-- Clock rates signals period in picoseconds

-- External clock source is 50MHz
type OutsideD = 'Dom "Raw" 20000

-- Pixel clock is 40MHz
type PixelD = 'Dom "HDMI" 25000
-- TMDS clock is 200MHz (5 * pixel clock)
type Pixelx5D  = 'Dom "HDMI" 5000

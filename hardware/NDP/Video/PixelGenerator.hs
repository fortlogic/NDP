{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module NDP.Video.PixelGenerator (PixelCoord (),
                                 pixelCounter,
                                 pixelControl,
                                 pixelGenerator) where

import Clash.Prelude

import NDP.Video.CBMColor
import NDP.Clocking.Domains
import NDP.Video.Timing

-- (Row, Column) or (y,x)
data PixelCoord = Px (Index 628) (Index 628)
                deriving (Show, Eq)

pixelCounter :: HiddenClockResetEnable dom => Signal dom VideoTime
pixelCounter = register videoTimeZero step
  where step = vidTick <$> pixelCounter

-- (Maybe (row, col), hSync, vSync)
pixelControl :: VideoTime -> (Maybe PixelCoord, Bit, Bit)
pixelControl t = (maybeCoord,
                  bitCoerce (hR == SyncR),
                  bitCoerce (vR == SyncR))
  where (VidRegion vR hR) = vidRegion t
        vT = vTime t
        hT = hTime t
        maybeCoord = if (vR == ImageR) && (hR == ImageR)
                     then Just $ Px vT (resize hT)
                     else Nothing

staticPixelGenerator :: PixelCoord -> RGBColor
staticPixelGenerator (Px y x) = if inFrameX && inFrameY
                                then cbmToRGB Blue
                                else cbmToRGB LightBlue
  where inFrameY = (y >= 44) && (y < 556)
        inFrameX = (x >= 144) && (x < 656)

pixelGenerator :: Signal Pixel (Maybe PixelCoord) -> Signal Pixel (Maybe RGBColor)
pixelGenerator coord = (staticPixelGenerator <$>) <$> coord

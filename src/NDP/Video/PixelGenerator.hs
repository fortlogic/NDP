{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module NDP.Video.PixelGenerator (PixelCoord (),
                           pixelCounter,
                           pixelCounter',
                           pixelControl,
                           pixelGenerator) where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import qualified Prelude as P

import NDP.Video.CBMColor
import NDP.Clocking
import NDP.Video.Timing

-- (Row, Column) or (y,x)
data PixelCoord = Px (Unsigned 10) (Unsigned 10)
                deriving (Show, Eq)

pixelCounter :: SignalPx VideoTime
pixelCounter = register' pxClk videoTimeZero step
  where step = vidTick <$> pixelCounter

pixelCounter' :: SignalPx a -> SignalPx VideoTime
pixelCounter' _ = pixelCounter

-- (Maybe (row, col), hSync, vSync)
pixelControl :: VideoTime -> (Maybe PixelCoord, Bit, Bit)
pixelControl t = (maybeCoord,
                  boolToBV (hR == SyncR),
                  boolToBV (vR == SyncR))
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

pixelGenerator :: SignalPx (Maybe PixelCoord) -> SignalPx (Maybe RGBColor)
pixelGenerator coord = (staticPixelGenerator <$>) <$> coord

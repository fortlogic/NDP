{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module VideoGenerator where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import qualified Prelude as P

import NDP.CBMColor
import NDP.Clocking
import NDP.TMDS
import NDP.VideoTiming

data PixelCoord = Px (Unsigned 10) (Unsigned 10)
                deriving (Show, Eq)

pixelCounter :: SignalPx VideoTime
pixelCounter = register' pxClk videoTimeZero step
  where step = vidTick <$> pixelCounter

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

tmdsLow :: TMDS
tmdsLow = TMDSControl 0

rgb2tmds :: Maybe RGBColor -> -- color
            Bit ->            -- hsync
            Bit ->            -- vsync
            Vec 3 TMDS        -- R G B
rgb2tmds Nothing h v = tmdsLow :> tmdsLow :> TMDSControl (v ++# h) :> Nil
rgb2tmds (Just (RGB r g b)) _ _ = r' :> g' :> b' :> Nil
  where r' = TMDSData r
        g' = TMDSData g
        b' = TMDSData b

rgb2Video :: SignalPx (Maybe RGBColor) ->
             SignalPx Bit ->
             SignalPx Bit ->
             SignalPx (Vec 3 (BitVector 10))
rgb2Video mc hSync vSync = bundle' pxClk (tmdsR :> tmdsG :> tmdsB :> Nil)
  where tmdsVec = unbundle' pxClk $ rgb2tmds <$> mc <*> hSync <*> vSync
        tmdsR = tmdsEncoder $ tmdsVec !! 0
        tmdsG = tmdsEncoder $ tmdsVec !! 1
        tmdsB = tmdsEncoder $ tmdsVec !! 2

generateVideo :: (SignalPx (Maybe PixelCoord) -> SignalPx (Maybe RGBColor)) ->
                 SignalPx (Vec 3 (BitVector 10))
generateVideo gen = rgb2Video rgb hSync vSync
  where timer = pixelCounter
        (maybeCoord, hSync, vSync) = unbundle' pxClk  $ pixelControl <$> timer
        rgb = gen maybeCoord
 

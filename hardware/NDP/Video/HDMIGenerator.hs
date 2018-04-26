{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module NDP.Video.HDMIGenerator where

import Clash.Prelude

import NDP.Video.CBMColor
import NDP.Clocking
import NDP.Video.PixelGenerator
import NDP.Video.TMDS

tmdsLow :: TMDS
tmdsLow = TMDSControl 0

rgb2tmds :: Maybe RGBColor -> -- color
            Bit ->            -- hsync
            Bit ->            -- vsync
            Vec 3 TMDS        -- R G B
rgb2tmds Nothing hrz vrt = tmdsLow :> tmdsLow :> TMDSControl (vrt ++# hrz) :> Nil
rgb2tmds (Just (RGB r g b)) _ _ = r' :> g' :> b' :> Nil
  where r' = TMDSData r
        g' = TMDSData g
        b' = TMDSData b

rgb2Video :: SignalPx (Maybe RGBColor) ->
             SignalPx Bit ->
             SignalPx Bit ->
             SignalPx (Vec 3 (BitVector 10))
rgb2Video mc hSync vSync = bundle (tmdsR :> tmdsG :> tmdsB :> Nil)
  where tmdsVec = unbundle $ rgb2tmds <$> mc <*> hSync <*> vSync
        tmdsR = tmdsEncoder $ at d0 tmdsVec
        tmdsG = tmdsEncoder $ at d1 tmdsVec
        tmdsB = tmdsEncoder $ at d2 tmdsVec

generateVideo :: (SignalPx (Maybe PixelCoord) -> SignalPx (Maybe RGBColor)) ->
                 SignalPx5 (Vec 3 (BitVector 2))
generateVideo gen = hdmi
  where timer = pixelCounter
        (maybeCoord, hSync, vSync) = unbundle $ pixelControl <$> timer
        rgb = gen maybeCoord
        tmdsVec = rgb2tmds <$> rgb <*> hSync <*> vSync
        wordVec = map tmdsEncoder $ unbundle tmdsVec
        hdmi = bundle $ map pxTo5x wordVec

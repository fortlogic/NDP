{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module NDP.Video.HDMIGenerator where

import Clash.Prelude

import NDP.Video.CBMColor
import NDP.Clocking.Domains
import NDP.Video.PixelGenerator
import NDP.Video.TMDS

tmdsLow :: TMDS
tmdsLow = TMDSControl 0

rgb2tmds :: Maybe RGBColor -> -- color
            Bit ->            -- hsync
            Bit ->            -- vsync
            Vec 3 TMDS        -- R G B
rgb2tmds Nothing hrz vrt = tmdsLow :> tmdsLow :> TMDSControl (pack vrt ++# pack hrz) :> Nil
rgb2tmds (Just (RGB r g b)) _ _ = r' :> g' :> b' :> Nil
  where r' = TMDSData r
        g' = TMDSData g
        b' = TMDSData b

rgb2Video :: Signal PixelD (Maybe RGBColor) ->
             Signal PixelD Bit ->
             Signal PixelD Bit ->
             Signal PixelD (Vec 3 (BitVector 10))
rgb2Video mc hSync vSync = bundle (tmdsR :> tmdsG :> tmdsB :> Nil)
  where tmdsVec = unbundle $ rgb2tmds <$> mc <*> hSync <*> vSync
        tmdsR = tmdsEncoder $ at d0 tmdsVec
        tmdsG = tmdsEncoder $ at d1 tmdsVec
        tmdsB = tmdsEncoder $ at d2 tmdsVec

generateVideo :: (Signal PixelD (Maybe PixelCoord) -> Signal PixelD (Maybe RGBColor)) ->
                 Signal Pixelx5D (Vec 3 (BitVector 2))
generateVideo gen = hdmi
  where timer = pixelCounter
        (maybeCoord, hSync, vSync) = unbundle $ pixelControl <$> timer
        rgb = gen maybeCoord
        tmdsVec = rgb2tmds <$> rgb <*> hSync <*> vSync
        wordVec = map tmdsEncoder $ unbundle tmdsVec
        hdmi = bundle $ map pxTo5x wordVec

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module NDP.Video.HDMIGenerator where

import Clash.Prelude

import NDP.Video.CBMColor
import NDP.Video.TMDS
-- import NDP.Video.PixelGenerator

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

rgb2Video :: HiddenClockResetEnable domain
          => Signal domain (Maybe RGBColor)
          -> Signal domain Bit
          -> Signal domain Bit
          -> Signal domain (Vec 3 (BitVector 10))
rgb2Video mc hSync vSync = bundle (tmdsR :> tmdsG :> tmdsB :> Nil)
  where tmdsVec = unbundle $ rgb2tmds <$> mc <*> hSync <*> vSync
        tmdsR = tmdsEncoder $ at d0 tmdsVec
        tmdsG = tmdsEncoder $ at d1 tmdsVec
        tmdsB = tmdsEncoder $ at d2 tmdsVec

-- generateVideo :: ( HiddenClockReset PixelD gated1 synchronous1
--                  , HiddenClockReset Pixel5xD gated2 synchronous2 )
--               => ( Signal PixelD (Maybe PixelCoord) -> Signal PixelD (Maybe RGBColor) )
--               -> Signal Pixel5xD (Vec 3 (BitVector 2))
-- generateVideo gen = undefined -- hdmi
--   where timer = pixelCounter
--         (maybeCoord, hSync, vSync) = unbundle $ pixelControl <$> timer
--         rgb = gen maybeCoord
--         tmdsVec = rgb2tmds <$> rgb <*> hSync <*> vSync
--         (wordR :> wordG :> wordB :> Nil) = map tmdsEncoder $ unbundle tmdsVec
--         -- hdmi = bundle $ (hideClock $ hideClockReset stretchBitVectorSlice) wordVec

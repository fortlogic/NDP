{-# LANGUAGE DataKinds #-}
module NDP.Video.CBMColor (RGBColor (..),
                     CBMColor (..),
                     cbmToRGB) where

import Clash.Prelude

data RGBColor = RGB (Unsigned 8) (Unsigned 8) (Unsigned 8)
                deriving (Show, Eq)

-- CBM Colors are based on the VIC-II video chip in the Commodore 64 & 128.
-- http://unusedino.de/ec64/technical/misc/vic656x/colors/index.html

data CBMColor = Black
              | White
              | Red
              | Cyan
              | Purple
              | Green
              | Blue
              | Yellow
              | Orange
              | Brown
              | LightRed
              | DarkGrey
              | Grey
              | LightGreen
              | LightBlue
              | LightGrey
              deriving (Show, Eq, Ord)


cbmToRGB :: CBMColor -> RGBColor
cbmToRGB Black      = RGB 0x00 0x00 0x00
cbmToRGB White      = RGB 0xFF 0xFF 0xFF
cbmToRGB Red        = RGB 0x68 0x37 0x2B
cbmToRGB Cyan       = RGB 0x70 0xA4 0xB2
cbmToRGB Purple     = RGB 0x6F 0x3D 0x86
cbmToRGB Green      = RGB 0x58 0x8D 0x43
cbmToRGB Blue       = RGB 0x35 0x28 0x79
cbmToRGB Yellow     = RGB 0xB8 0xC7 0x6F
cbmToRGB Orange     = RGB 0x6F 0x4F 0x25
cbmToRGB Brown      = RGB 0x43 0x39 0x00
cbmToRGB LightRed   = RGB 0x9A 0x67 0x59
cbmToRGB DarkGrey   = RGB 0x44 0x44 0x44
cbmToRGB Grey       = RGB 0x6C 0x6C 0x6C
cbmToRGB LightGreen = RGB 0x9A 0xD2 0x84
cbmToRGB LightBlue  = RGB 0x6C 0x5E 0xB5
cbmToRGB LightGrey  = RGB 0x95 0x95 0x95

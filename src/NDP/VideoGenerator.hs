module VideoGenerator where

import CLaSH.Prelude

import NDP.VideoTiming

pixelCounter :: Signal VideoTime
pixelCounter = register videoTimeZero step
  where step = vidTick <$> pixelCounter

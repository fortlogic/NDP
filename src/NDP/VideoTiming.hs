{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NDP.VideoTiming where

import CLaSH.Prelude

-- 40MHz Pixel clock

-- Horizontal Pixel Timing:
-- + Region :  |<-PixelData->|<-FrontPorch->|<--hSync-->|<-backPorch->|<--...
-- + Length :  |<----800---->|<-----40----->|<---128--->|<-----88---->|<--...
-- + Clock  :  |<-0     799->|<-800    839->|<-840 967->|<-968  1055->|<-1056

-- Vertical Pixel Timing:
-- + Region : |<-PixelData->|<-FrontPorch->|<--vSync-->|<-backPorch->|<-...
-- + Length : |<----600---->|<------1----->|<----4---->|<-----23---->|<-...
-- + Clock  : |<-0     599->|<=====600====>|<-601 604->|<-605   627->|<-628

-- Video timing is hardcoded into each function because these were written just
-- after watching the first 2016 presidential debate. Being able to live with
-- myself after hearing Donald Trump's voice for that long required a sizable
-- amount of Gin, so the blame for any strange senseless architectural decisions
-- are to be placed squarely on his orange pasty lap.

data Control = ImageC
             | PreSyncC
             | SyncC
             | PostSyncC
             deriving (Show, Eq, Ord)

data VideoTime = VidTime {
  vTime :: Unsigned 10,
  hTime :: Unsigned 11
  } deriving (Show, Eq, Ord)

data VideoControl = VControl {
  vControl :: Control,
  hControl :: Control
  } deriving (Show, Eq, Ord)

videoTick :: VideoTime -> VideoTime
videoTick (VidTime 627 1055) = VidTime 0     0
videoTick (VidTime v   1055) = VidTime (v+1) 0
videoTick (VidTime v   h)    = VidTime v     (h+1)

vidControl :: VideoTime -> VideoControl
vidControl (VidTime v h) = VControl (horizontal h) (vertical v)
  where horizontal h | h < 800  = ImageC
        horizontal h | h < 840  = PreSyncC
        horizontal h | h < 968  = SyncC
        horizontal h | h < 1056 = PostSyncC
        vertical v | v < 600 = ImageC
        vertical v | v < 601 = PreSyncC
        vertical v | v < 605 = SyncC
        vertical v | v < 628 = PostSyncC

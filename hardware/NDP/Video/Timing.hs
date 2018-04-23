{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NDP.Video.Timing (TimeRegion (..),
                        VideoTime,
                        VideoRegion (..),
                        videoTimeZero,
                        vTime,
                        hTime,
                        vidTick,
                        vidRegion) where

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
-- amount of gin, so the blame for any strange senseless architectural decisions
-- are to be placed squarely on his orange pasty lap.

data TimeRegion = ImageR
                | PreSyncR
                | SyncR
                | PostSyncR
                deriving (Show, Eq, Ord)

data VideoTime = VidTime {
  vTime :: Unsigned 10,
  hTime :: Unsigned 11
  } deriving (Show, Eq, Ord)

data VideoRegion = VidRegion {
  vRegion :: TimeRegion,
  hRegion :: TimeRegion
  } deriving (Show, Eq, Ord)

videoTimeZero :: VideoTime
videoTimeZero = VidTime 0 0

vidTick :: VideoTime -> VideoTime
vidTick (VidTime 627 1055) = VidTime 0     0
vidTick (VidTime v   1055) = VidTime (v+1) 0
vidTick (VidTime v   h)    = VidTime v     (h+1)

vidRegion :: VideoTime -> VideoRegion
vidRegion (VidTime v h) = VidRegion (horizontal h) (vertical v)
  where horizontal h | h < 800  = ImageR
        horizontal h | h < 840  = PreSyncR
        horizontal h | h < 968  = SyncR
        horizontal h | h < 1056 = PostSyncR
        vertical v | v < 600 = ImageR
        vertical v | v < 601 = PreSyncR
        vertical v | v < 605 = SyncR
        vertical v | v < 628 = PostSyncR

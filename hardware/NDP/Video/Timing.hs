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
  vTime :: Index 628,
  hTime :: Index 1056
  } deriving (Show, Eq, Ord)

data VideoRegion = VidRegion {
  vRegion :: TimeRegion,
  hRegion :: TimeRegion
  } deriving (Show, Eq, Ord)

videoTimeZero :: VideoTime
videoTimeZero = VidTime 0 0

vidTick :: VideoTime -> VideoTime
vidTick (VidTime 627 1055) = VidTime 0       0
vidTick (VidTime ver 1055) = VidTime (ver+1) 0
vidTick (VidTime ver hori) = VidTime ver     (hori+1)

vidRegion :: VideoTime -> VideoRegion
vidRegion (VidTime vt hr) = VidRegion (horizontal hr) (vertical vt)
  where horizontal hori | hori < 800  = ImageR
        horizontal hori | hori < 840  = PreSyncR
        horizontal hori | hori < 968  = SyncR
        horizontal _    | otherwise   = PostSyncR
        vertical vert | vert < 600 = ImageR
        vertical vert | vert < 601 = PreSyncR
        vertical vert | vert < 605 = SyncR
        vertical _    | otherwise  = PostSyncR

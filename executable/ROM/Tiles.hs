module ROM.Tiles where

import Data.List
import Data.Maybe

data TileMap = TileMap {
  defaultTile :: TileData,
  tiles :: [Tile]
  } deriving (Read, Show)

data Tile = Tile {
  name :: String,
  code :: Int,
  content :: TileData
  } deriving (Eq, Read, Show)

data TileData = Empty
              | Full
              | File FilePath
              deriving (Eq, Read, Show)

-- This is really really slow
verifyMap :: TileMap -> Bool
verifyMap tileMap = all uniqueTile (tiles tileMap)
  where uniqueTile = (1==) . length . sameTiles
        sameTiles t = [ t' | t' <- tiles tileMap,  (code t) == (code t')]

-- This is really slow
mapTile :: TileMap -> Int -> TileData
mapTile tileMap idx = fromMaybe (defaultTile tileMap) maybeTileData
  where maybeTileData = content <$> find ((idx==) . code) (tiles tileMap)


{-# LANGUAGE DataKinds #-}
module Make.TileROM (tileROMRules,
                     buildTileROM) where

import qualified Data.ByteString.Char8 as C
import Data.Conf

import Data.Maybe
import qualified Data.Vector.Storable as V
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import Graphics.Netpbm

import Make.Config
import Make.PPM
import ROM.Tiles

tileROMRules :: Rules ()
tileROMRules = do
  buildDir <- fromJust <$> getConfigIO "TILE_OUT"
  (buildDir </> "*") %> \ rom -> do
    let mapName = takeBaseName rom
    tileMap <- fromJust <$> getConfig "TILE_MAP"
    need [tileMap]
    buildTileROM tileMap mapName rom

getTileMap :: String -> Conf -> Maybe TileMap
getTileMap mapName conf = getConf mapName conf

buildTileROM :: FilePath -> String -> FilePath -> Action ()
buildTileROM mapFile tileSet romPath = do
  -- parse the map file
  maybeMap <- getTileMap <$> pure tileSet <*> liftIO (readConf mapFile)
  case verifyMap <$> maybeMap of -- error if something is wrong
    Nothing -> error ("* " ++ mapFile ++ " : unable to parse")
    Just False -> error ("* " ++ mapFile ++ " : invalid tile map")
    _ -> return ()
  -- extract a list of tile data (for each character)
  let tileMap = fromJust maybeMap
  let tilesContent = [ mapTile tileMap idx | idx <- [0..255] ]
  -- grab the bytes and write them out
  liftIO $ do
    romBytes <- tiles2bytes (takeDirectory mapFile) tilesContent
    C.writeFile romPath romBytes

-- This whole everything is extremely inefficient...
tiles2bytes :: FilePath -> [TileData] -> IO C.ByteString
tiles2bytes root ts = C.intercalate (C.pack "\n") <$> sequence tileBytes
  where tileBytes = map (tile2bytes root) ts

tile2bytes :: FilePath -> TileData -> IO C.ByteString
tile2bytes root td = C.intercalate (C.pack "\n") <$> tileLines
  where tileLines = sequence [ tileDataLine root td ln | ln <- [0..7] ]

-- Returns ROM source file line given some TileData and a line index
tileDataLine :: FilePath -> TileData -> Int -> IO C.ByteString
tileDataLine _ Empty _ = return $ C.pack "00000000"
tileDataLine _ Full _ = return $ C.pack "11111111"
tileDataLine root (File path) line = do
--  Right ([tilePPM], _) <- parsePPM <$> C.readFile
  maybeTilePPM <- readPPM (root </> path)
  let tilePPM = fromMaybe (error $ "Invalid PBM image: " ++ path) maybeTilePPM
  case verifyTilePPM tilePPM of
    Nothing -> error ("Invalid Tile Image: " ++ path)
    Just (pxs) -> do
      let pxLine = pixelVectorToList $ V.slice (line * 8) 8 pxs
      return $ C.pack $ map pixel2Char pxLine

verifyTilePPM :: PPM -> Maybe (V.Vector PbmPixel)
verifyTilePPM (PPM (PPMHeader P1 8 8) (PbmPixelData d)) = Just d
verifyTilePPM (PPM (PPMHeader P4 8 8) (PbmPixelData d)) = Just d
verifyTilePPM _ = Nothing

pixel2Char :: PbmPixel -> Char
pixel2Char (PbmPixel True) = '0'
pixel2Char (PbmPixel False) = '1'

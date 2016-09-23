module Main where

import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import Development.Shake.Util

import Config
import Oracles
import TileROM

ghcFlags :: Action [String]
ghcFlags = do
  clashVer <- askOracleWith (ClashVersion ()) ""
  odir <- configFlag2 "-odir" "CLASH_ODIR"
  hidir <- configFlag2 "-hidir" "CLASH_HIDIR"
  idir <- configFlag "-i" "CLASH_INCLUDES"
  hdldir <- configFlag2 "-clash-hdldir" "CLASH_HDLDIR"
  return (odir ++ hidir ++ [idir] ++ hdldir)

clashExec = "stack exec clash --"

b2src :: FilePath -> FilePath
b2src f = "src" </> (dropDirectory1 $ f -<.> "hs")

hsDeps :: [(FilePath, [FilePath])] -> [FilePath]
hsDeps = filter isHs . concat . map snd
  where isHs = isSuffixOf ".hs"

main :: IO ()
main = shakeArgs shakeOptions $ do
  installOracles
  setupConfig "NDP.config"
  phony "setup" $ do
    cmd "stack build clash-ghc"

  phony "clean" $ do
    cmd "rm -rvf build"

  phony "test" $ return ()

  "build/clash/vhdl/*/*_topentity.vhdl" %> \ top -> do
    clashDir <- maybeConfig "CLASH_SRC" ""
    let name = (splitDirectories top !! 3)
    let src = clashDir </> name <.> "hs"

    -- generate the correct dependencies
    let makefilePath = "build" </> "clash" </> name -<.> "mk"
    need [makefilePath]
    makefile <- parseMakefile <$> readFile' makefilePath
    need (hsDeps makefile)

    -- compile
    flags <- ghcFlags
    cmd clashExec flags "--vhdl" src

  "build/clash//*.mk" %> \ mk -> do
    alwaysRerun
    let hsfile = b2src mk -<.> "hs"
    flags <- ghcFlags
    cmd clashExec "-M -dep-suffix=" [""] " -dep-makefile" [mk] flags hsfile

  "build/ROM/tile/*.rom" %> \ rom -> do
    let mapName = takeBaseName rom
    tileMap <- fromJust <$> getConfig "TILE_MAP"
    need [tileMap]
    buildTileROM tileMap mapName rom

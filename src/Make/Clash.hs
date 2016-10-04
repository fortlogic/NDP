module Make.Clash (clashRules) where

import Data.List
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util

import Make.Config
import Make.Oracles
import Make.Utils

clashExec = "stack exec clash --"

ghcFlags :: Action [String]
ghcFlags = do
  clashVer <- askOracleWith (ClashVersion ()) ""
  odir <- configFlag2 "-odir" "CLASH_ODIR"
  hidir <- configFlag2 "-hidir" "CLASH_HIDIR"
  idir <- configFlag "-i" "CLASH_INCLUDES"
  hdldir <- configFlag2 "-clash-hdldir" "CLASH_HDLDIR"
  return (odir ++ hidir ++ [idir] ++ hdldir)

hsDeps :: [(FilePath, [FilePath])] -> [FilePath]
hsDeps = filter isHs . concat . map snd
  where isHs = isSuffixOf ".hs"

clashRules = do
  buildDir <- liftIO $ maybeConfigIO "BUILD_DIR" "build"
  clashOut <- liftIO $ maybeConfigIO "CLASH_OUT" (buildDir </> "clash")
  
  (clashOut </> "vhdl//*_topentity.vhdl") %> \ top -> do
    clashDir <- maybeConfig "CLASH_SRC" ""
    let clashOutLen = length $ splitDirectories clashOut
    let sharedPath = (withPath $ withReverse tail . tail . drop clashOutLen) top
    let src = clashDir </> sharedPath -<.> "hs"
    let mk = clashOut </> "mk" </> sharedPath -<.> "mk"

    -- generate the correct dependencies
    need [mk]
    makefile <- parseMakefile <$> readFile' mk
    need (hsDeps makefile)

    -- compile
    flags <- ghcFlags
    cmd clashExec flags "--vhdl" src

  clashOut </> "mk//*.mk" %> \ mk -> do
    alwaysRerun
    clashDir <- maybeConfig "CLASH_SRC" ""
    let clashOutLen = length $ splitDirectories clashOut
    let sharedPath = (joinPath . tail . drop clashOutLen . splitDirectories) mk
    let src = clashDir </> sharedPath -<.> "hs"
    flags <- ghcFlags
    cmd clashExec "-M -dep-suffix=" [""] " -dep-makefile" [mk] flags src

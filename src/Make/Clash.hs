module Make.Clash (clashRules) where

import Data.List
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util

import Make.Config
import Make.Oracles

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
  
  (clashOut </> "vhdl/*/*_topentity.vhdl") %> \ top -> do
    clashDir <- maybeConfig "CLASH_SRC" ""
    let name = (splitDirectories top !! 3)
    let src = clashDir </> name <.> "hs"

    -- generate the correct dependencies
    let makefilePath = clashOut </> name -<.> "mk"
    need [makefilePath]
    makefile <- parseMakefile <$> readFile' makefilePath
    need (hsDeps makefile)

    -- compile
    flags <- ghcFlags
    cmd clashExec flags "--vhdl" src

  clashOut </> "*.mk" %> \ mk -> do
    alwaysRerun
    clashDir <- maybeConfig "CLASH_SRC" ""
    let hsfile = (replaceDirectory mk clashDir) -<.> "hs"
    flags <- ghcFlags
    cmd clashExec "-M -dep-suffix=" [""] " -dep-makefile" [mk] flags hsfile

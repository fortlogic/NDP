module Make.Clash (clashRules) where

import Data.List
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import Development.Shake.Util
import System.IO.Temp

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
  return (odir ++ hidir ++ [idir])

hsDeps :: [(FilePath, [FilePath])] -> [FilePath]
hsDeps = filter isHs . concat . map snd
  where isHs = isSuffixOf ".hs"

clashRules = do
  buildDir <- liftIO $ maybeConfigIO "BUILD_DIR" "build"
  clashOut <- liftIO $ maybeConfigIO "CLASH_OUT" (buildDir </> "clash")

  (clashOut </> "*/ndp_topentity.vhdl") %> \ vhdlF -> do
    let vhdlD = takeDirectory vhdlF
    let mkF = vhdlD <.> "mk"
    need [mkF]
    makefile <- parseMakefile <$> readFile' mkF
    need (hsDeps makefile)
    withTempDir $ \ tmpD -> do
      (Just entityD) <- getConfig "CLASH_TOPENTITIES"
      let srcF = entityD </> takeFileName mkF -<.> "hs"
      flags <- ghcFlags
      () <- cmd clashExec flags "-clash-hdldir" tmpD "--vhdl" srcF
      () <- cmd "rm -rf" vhdlD
      cmd "mv" (tmpD </> "vhdl" </> "NDP") vhdlD

  (clashOut </> "*.mk") %> \ mkF -> do
    alwaysRerun
    (Just entityD) <- getConfig "CLASH_TOPENTITIES"
    let srcF = entityD </> takeFileName mkF -<.> "hs"
    flags <- ghcFlags
    cmd clashExec "-M -dep-suffix=" [""] " -dep-makefile" [mkF] flags srcF

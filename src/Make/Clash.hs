module Make.Clash (clashRules) where

import Data.List
import Development.Shake
import Development.Shake.Config
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
  return (odir ++ hidir ++ [idir])

hsDeps :: [(FilePath, [FilePath])] -> [FilePath]
hsDeps = filter isHs . concat . map snd
  where isHs = isSuffixOf ".hs"

clashRules = do
  buildDir <- liftIO $ maybeConfigIO "BUILD_DIR" "build"
  clashOut <- liftIO $ maybeConfigIO "CLASH_OUT" (buildDir </> "clash")

  (clashOut </> "*/*.vhdl") %> \ vhdlF -> do
    let vhdlD = takeDirectory vhdlF

    let mkF = vhdlD <.> "mk"
    (Just entityD) <- getConfig "TOPLEVEL_ENTITIES"
    (Just mainClashNameF) <- getConfig "TOPLEVEL_HS_FILE"
    let srcF = entityD </> takeBaseName mkF </> mainClashNameF -<.> "hs"
    flags <- ghcFlags
    withTempFile $  \ mkF' -> do
      putNormal "Determining CLaSH dependencies"
      () <- cmd clashExec "-M -dep-suffix=" [""] " -dep-makefile" [mkF'] flags srcF

      -- lns <- readFileLines mkF' -- We don't want this tracked as a dependency
      lns <- lines <$> liftIO (readFile mkF')
      writeFileLines mkF [ln | ln <- lns, isSuffixOf ".hs" ln]

    needMakefileDependencies mkF

    withTempDir $ \ tmpD -> do
      (Just entityD) <- getConfig "TOPLEVEL_ENTITIES"
      (Just clashName) <- getConfig "CLASH_ENTITY_NAME"
      -- let srcF = entityD </> takeFileName mkF -<.> "hs"
      flags <- ghcFlags
      putNormal "Compiling CLaSH sources"
      () <- cmd clashExec flags "-clash-hdldir" tmpD "--vhdl" srcF
      () <- cmd "rm -rf" vhdlD
      putNormal tmpD
      cmd "mv" (tmpD </> "vhdl" </> clashName) vhdlD

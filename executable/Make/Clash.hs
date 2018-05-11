module Make.Clash (clashRules) where

import Control.Monad.IO.Class
import Data.List
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import Development.Shake.Util

import Make.Config
import Make.HDL

clashExec :: String
clashExec = "stack exec clash --"

ghcFlags :: Action [String]
ghcFlags = do
  odir <- configFlag2 "-odir" "CLASH_ODIR"
  hidir <- configFlag2 "-hidir" "CLASH_HIDIR"
  idir <- configFlag "-i" "SRC"
  primDir <- configFlag "-i" "HDL_PRIMITIVES"
  return (odir ++ hidir ++ [idir] ++ [primDir])

getBuildDir :: MonadIO m => m String
getBuildDir = liftIO $ maybeConfigIO "BUILD" "build"

getClashOut :: MonadIO m => m String
getClashOut = getBuildDir >>= fetch
  where fetch buildDir = liftIO $ maybeConfigIO "CLASH_OUT" (buildDir </> "clash")

clashRules :: Rules ()
clashRules = do

  -- This is cheating but since the locations of the targets are determined by
  -- the config file...
  clashOut <- getClashOut

  (clashOut </> "vhdl/*/*/*.vhdl") %> buildHDL VHDL
  (clashOut </> "verilog/*/*/*.v") %> buildHDL Verilog


buildHDL :: HDL -> FilePath -> Action ()
buildHDL hdl hdlF = do
  let hdlD = takeDirectory hdlF

  clashOut <- getClashOut

  -- TODO: we depend on any HDL primitives.
  -- (Just primitiveD) <- getConfig "VHDL_PRIMITIVES"
  -- primitiveFs <- getDirectoryFiles primitiveD ["*.json"]

  -- getDirectoryFiles implicitly `need`s the results so we don't need to do
  -- it.

  -- GHC has the capability to take a haskell source tree and spit out an old
  -- fashioned makefile for compiling any file in that tree. This lets us make
  -- explicit all the modules that the top level entity (transitively)
  -- imports. Since Shake has stuff for parsing makefile dependencies this
  -- lets us know what files we need to depend on to (potentially) trigger a
  -- rebuild.

  let mkF = hdlD <.> "mk"
  (Just entityD) <- getConfig "HDL_ENTITIES"
  (Just mainClashNameF) <- getConfig "TOPLEVEL_HS_FILE"
  let srcF = entityD </> takeBaseName mkF </> mainClashNameF -<.> "hs"
  flags <- ghcFlags

  -- We need to muck about with the makefile after clash spits it out, so
  -- stick it in a temporary file.
  withTempFile $  \ mkF' -> do
    putNormal "Determining CLaSH dependencies"
    () <- cmd clashExec "-M -dep-suffix=" [""] " -dep-makefile" [mkF'] flags srcF

    -- We're lifting from IO becdause readFileLines will add the temporary make
    -- file as an additional dependency, which we do not want. It is a temporary
    -- file, after all.
    lns <- lines <$> liftIO (readFile mkF')
    writeFileLines mkF [ln | ln <- lns, isSuffixOf ".hs" ln]

  -- Actually include the dependencies.
  needMakefileDependencies mkF

  -- generate the hdl
  putNormal "Compiling CLaSH sources"
  cmd clashExec flags "-outputdir" clashOut (hdlFlag hdl) srcF

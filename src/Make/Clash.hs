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
  idir <- configFlag "-i" "SRC"
  primDir <- configFlag "-i" "HDL_PRIMITIVES"
  return (odir ++ hidir ++ [idir] ++ [primDir])

hsDeps :: [(FilePath, [FilePath])] -> [FilePath]
hsDeps = filter isHs . concat . map snd
  where isHs = isSuffixOf ".hs"

data HDL = VHDL | Verilog deriving (Read, Show, Eq)

clashRules = do

  -- This is cheating but since the locations of the targets are determined by
  -- the config file...
  buildDir <- liftIO $ maybeConfigIO "BUILD" "build"
  clashOut <- liftIO $ maybeConfigIO "CLASH_OUT" (buildDir </> "clash")

  (clashOut </> "*/*.vhdl") %> buildHDL undefined


buildHDL :: HDL -> FilePath -> Action ()
buildHDL hdl hdlF = do
  let hdlD = takeDirectory hdlF

  -- we depend on any HDL primitives.
  (Just primitiveD) <- getConfig "HDL_PRIMITIVES"
  primitiveFs <- getDirectoryFiles primitiveD ["*.json"]
  -- getDirectoryFiles implicitly `need`s the results so we don't need to do
  -- it.

  -- GHC has the capability to take a haskell source tree and spit out an old
  -- fashioned makefile for compiling any file in that tree. This lets us make
  -- explicit all the modules that the top level entity (transitively)
  -- imports. Since Shake has stuff for parsing makefile dependencies this
  -- lets us know what files we need to depend on to (potentially) trigger a
  -- rebuild.

  let mkF = hdlD <.> "mk"
  (Just entityD) <- getConfig "TOPLEVEL_ENTITIES"
  (Just mainClashNameF) <- getConfig "TOPLEVEL_HS_FILE"
  let srcF = entityD </> takeBaseName mkF </> mainClashNameF -<.> "hs"
  flags <- ghcFlags

  -- We need to muck about with the makefile after clash spits it out, so
  -- stick it in a temporary file.
  withTempFile $  \ mkF' -> do

    putNormal "Determining CLaSH dependencies"
    () <- cmd clashExec "-M -dep-suffix=" [""] " -dep-makefile" [mkF'] flags srcF

    -- We're lifting from IO becdause readFileLines will add the temporary
    -- make file as a dependency, which we do not want. It is a temporary
    -- file, after all.
    lns <- lines <$> liftIO (readFile mkF')
    writeFileLines mkF [ln | ln <- lns, isSuffixOf ".hs" ln]


  -- Actually include the dependencies.
  needMakefileDependencies mkF

  -- I'm just treating the bunch of VHDL that clash spits out as an atomic
  -- blob. Remove the previously generated VHDL files and put the new ones in
  -- their place.
  withTempDir $ \ tmpD -> do

    (Just entityD) <- getConfig "TOPLEVEL_ENTITIES"
    (Just clashName) <- getConfig "CLASH_ENTITY_NAME"

    flags <- ghcFlags

    -- generate the hdl
    putNormal "Compiling CLaSH sources"
    () <- cmd clashExec flags "-clash-hdldir" tmpD "--vhdl" srcF

    -- get rid of the old
    () <- cmd "rm -rf" hdlD

    -- and replace
    cmd "mv" (tmpD </> "vhdl" </> clashName) hdlD

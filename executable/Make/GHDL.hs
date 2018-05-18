module Make.GHDL (ghdlRules) where

import Control.Monad.IO.Class
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import System.Directory

import Make.Config
import Make.Utils
import Make.HDL

getBuildDir :: MonadIO m => m String
getBuildDir = liftIO $ maybeConfigIO "BUILD" "build"

getGHDLOut :: MonadIO m => m String
getGHDLOut = getBuildDir >>= fetch
  where fetch buildDir = liftIO $ maybeConfigIO "GHDL_OUT" (buildDir </> "clash")

validTestbenchOutput :: Rules (FilePath -> Bool)
validTestbenchOutput = do
  ghdlOut <- getGHDLOut
  (Just entityD) <- liftIO $ getConfigIO "HDL_ENTITIES"
  entities <- liftIO $ getDirectoryDirsIO entityD
  return (\ testbenchF -> let testbenchF' = makeRelative ghdlOut testbenchF in
                      elem testbenchF' (map (\ entity -> entity </> "manifest" <.> "txt") entities))

ghdlRules :: Rules ()
ghdlRules = do
  (Just ghdlDir) <- liftIO $ getConfigIO "GHDL_OUT"

  (ghdlDir </> "*/work-obj93.cf") %> \ incF -> do
    let workD = takeDirectory incF
    let entityName = withPath (pure . last) $ workD
    (Just clashOutD) <- getConfig "CLASH_OUT"
    let clashVhdlD = clashOutD </> entityName
    clashVhdlAbsD <- liftIO $ makeAbsolute clashVhdlD
    need [clashVhdlD </> "ndp_topentity" <.> "vhdl"]
    clashVhdlFs <- liftIO $ getDirectoryFilesWithExt clashVhdlAbsD ".vhdl"
    cmd "ghdl" "-i" ("--workdir=" ++ workD) clashVhdlFs

  (ghdlDir </> "*/testbench") %> \ tbF -> do
    let workD = takeDirectory tbF
    need [workD </> "work-obj93.cf"]
    cmd "ghdl" ["-m",
                "--workdir=" ++ workD,
                "--ieee=synopsys",
                "-fexplicit",
                "-o", workD </> "testbench",
                "ndp_testbench"]

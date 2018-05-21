module Make.GHDL ( ghdlRules ) where

import Control.Monad.IO.Class
import Data.List
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import System.Directory

import Make.Clash
import Make.Config
import Make.HDL
import Make.Utils

-- ghdl -i --workdir=build/i386_Darwin/ghdl/PrimitiveTest --work=clockStrobe build/i386_Darwin/clash/vhdl/PrimitiveTest/clockStrobe/*.vhdl

ghdlRules :: Rules ()
ghdlRules = do
  ghdlDir <- getGHDLDir

  (ghdlDir </> "*/*-obj93.cf") %> \ incF -> do
    let path = stripPrefix ghdlDir incF
    let projectName = (takeFileName . takeDirectory) incF
    let (Just libName) = (stripSuffix "-obj93.cf" . takeFileName) incF
    generateIncludeFile projectName libName


  -- (ghdlDir </> "*/work-obj93.cf") %> \ incF -> do
  --   let workD = takeDirectory incF
  --   let entityName = withPath (pure . last) $ workD
  --   (Just clashOutD) <- getConfig "CLASH_OUT"
  --   let clashVhdlD = clashOutD </> entityName
  --   clashVhdlAbsD <- liftIO $ makeAbsolute clashVhdlD
  --   need [clashVhdlD </> "ndp_topentity" <.> "vhdl"]
  --   clashVhdlFs <- liftIO $ getDirectoryFilesWithExt clashVhdlAbsD ".vhdl"
  --   cmd "ghdl" "-i" ("--workdir=" ++ workD) clashVhdlFs

  (ghdlDir </> "*/testbench") %> \ tbF -> do
    putQuiet "OOPS"
    -- TODO: instead of all the junk below, generate a GHDLSpec
    -- then pass it to generateExecutableFiles

    -- let workD = takeDirectory tbF
    -- need [workD </> "work-obj93.cf"]
    -- cmd "ghdl" ["-m",
    --             "--workdir=" ++ workD,
    --             "--ieee=synopsys",
    --             "-fexplicit",
    --             "-o", workD </> "testbench",
    --             "ndp_testbench"]

getGHDLLibraries :: String -> Action [String]
getGHDLLibraries project = do
  manifest <- manifestPath VHDL project
  need [manifest]
  let vhdlD = takeDirectory manifest
  map takeFileName <$> getDirectoryDirs vhdlD

generateIncludeFile :: String -> String -> Action ()
generateIncludeFile project library = do
  clashOut <- getClashDir
  ghdlOut <- getGHDLDir
  need [ clashOut </> "vhdl" </> project </> "manifest.txt" ]

  cmd "ghdl" [ "-i"
             , "--work=" ++ library
             , "--workdir=" ++ ghdlOut </> project
             ]

generateExecutableFile :: String -> String -> String -> Action ()
generateExecutableFile project library entity = do
  putQuiet "NOT FINISHED WRITING THIS, needs to dispatch back to shake"
  generateIncludeFile project library

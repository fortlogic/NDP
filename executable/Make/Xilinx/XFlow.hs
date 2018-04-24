module Make.Xilinx.XFlow (xflowRules) where

import Data.Char
import Data.Conf
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import System.Posix.Escape

import Make.Config

import Make.Vagrant

xflowRules :: Rules ()
xflowRules = do
  (Just xilinxD) <- liftIO $ getConfigIO "XILINX_OUT"

  (xilinxD </> "*.prj") %> \ prjF -> do
    let entityName = takeBaseName prjF

    (Just clashOutD) <- getConfig "CLASH_OUT"
    let clashVhdlD = clashOutD </> entityName

    -- fetch the custom VHDL files that will be included in the project.
    (Just topLevelVhdlD) <- getConfig "TOPLEVEL_ENTITIES"
    customVhdlFs <- getDirectoryFiles "" [topLevelVhdlD </> entityName </> "*" <.> "vhdl"]

    -- fetch the VHDL files that the clash compiler generated.
    (Just clashEntity) <- getConfig "CLASH_ENTITY_NAME"
    need [clashVhdlD </> ( map toLower clashEntity ++ "_topentity.vhdl")]
    clashVhdlFs <- getDirectoryFiles "" [clashVhdlD  </> "*" <.> "vhdl"]

    -- fetch a clocking entity if one is specified
    (Just clockDir) <- getConfig "VHDL_CLOCKS"
    (Just entityD) <- getConfig "TOPLEVEL_ENTITIES"
    (Just configF) <- getConfig "ENTITY_CONFIG_SETTINGS"
    entityConstraints <- (liftIO . readConf) $ entityD </> entityName </> configF
    let maybeClockVhdl = do
          clockFile <- (getConf "clock" entityConstraints) :: Maybe String
          return $ clockDir </> clockFile -<.> "vhdl"

    -- fetch the global vhdl files
    (Just globalVhdlD) <- getConfig "GLOBAL_VHDL"
    globalVhdlFs <- getDirectoryFiles "" [globalVhdlD </> "*" <.> "vhdl"]

    (Just vmPrefix) <- getConfig "VM_ROOT"

    let vhdlFs = customVhdlFs ++ clashVhdlFs ++ maybeToList maybeClockVhdl ++ globalVhdlFs

    writeFileLines prjF ["vhdl \"" ++ (vmPrefix </> vhdlF) ++ "\"" | vhdlF <- vhdlFs]

  (xilinxD </> "*.bit") %> \ bitF -> do
    need [bitF -<.> "prj",
          bitF -<.> "ucf"]

    let entityName = takeBaseName bitF
    let entityVhdlName = entityName -- map toLower entityName

    let workD = dropExtension bitF
    () <- cmd "mkdir -p" workD

    (Just vmPrefix) <- getConfig "VM_ROOT"
    (Just xilinxPart) <- getConfig "XILINX_PART"

    (Just settingsF) <- getConfig "XILINX_SETTINGS"

    (Just xstOptF) <- getConfig "XST_OPT"
    (Just xflowFastF) <- getConfig "XFLOW_FAST"
    (Just bitgenOptF) <- getConfig "XFLOW_BITGEN_OPT"

    -- Move necessary files into work directory
    () <- cmd "cp -f" (bitF -<.> "ucf") (workD ++ "/")
    -- () <- cmd "cp -f" xstOptF (workD ++ "/")
    () <- cmd "cp -f" xflowFastF (workD ++ "/")
    () <- cmd "cp -f" bitgenOptF (workD ++ "/")

    -- Add top level entity option to xst opt file and copy to work directory
    xstOptLines <- readFileLines xstOptF
    let optPre = takeWhile (not . isPrefixOf "\"run\";") (reverse xstOptLines)
    let optSuf = dropWhile (not . isPrefixOf "\"run\";") (reverse xstOptLines)
    let xstOptLines' = reverse (optPre ++ ["\"-top " ++ entityVhdlName ++ "\";"] ++ optSuf)

    writeFileLines (workD </> takeFileName xstOptF) xstOptLines'

    () <- cmd "rm -f" (workD </> (takeBaseName bitF ++ "_map.ncd"))

    withVagrant $ do
      let xstOpt = takeFileName xstOptF
      let xflowFast = takeFileName xflowFastF
      let bitgenOpt = takeFileName bitgenOptF

      let sshCmd = escape $ intercalate " " ["source", settingsF, ";",
                                             "xflow",
                                             "-wd", vmPrefix </> workD,
                                             "-p", xilinxPart,
                                             "-synth", xstOpt,
                                             "-implement", xflowFast,
                                             "-config", bitgenOpt,
                                             vmPrefix </> bitF -<.> "prj"]

      () <- cmd Shell "vagrant ssh -c" [sshCmd]
      cmd "cp" (workD </> takeFileName bitF) bitF

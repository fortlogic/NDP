{-# LANGUAGE TypeApplications #-}
module Make.Xilinx.XFlow (xflowRules) where

import Data.Conf
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import System.Posix.Escape

import Make.Config
import Make.HDL
import Make.Utils
import Make.Vagrant

xflowRules :: Rules ()
xflowRules = do
  (Just xilinxD) <- liftIO $ getConfigIO "XILINX_OUT"

  (xilinxD </> "*.prj") %> \ prjF -> do
    let entityName = takeBaseName prjF

    (Just hdl) <- (readConfig @ HDL) "PREFERRED_HDL"
    (Just clashOutD) <- getConfig "CLASH_OUT"
    let clashHdlD = clashOutD </> entityName </> hdlName hdl
    let hdlExt = hdlExtension hdl

    -- fetch the HDL files that the clash compiler generated.
    (Just clashEntity) <- getConfig "CLASH_ENTITY_NAME"
    --need [clashHdlD </> ( map toLower clashEntity ++ "_topentity") <.> hdlExt]
    need [ clashHdlD </> entityName <.> hdlExt ]
    clashHdlFs <- getDirectoryFiles "" [clashHdlD  </> "*" <.> hdlExt]

    -- fetch a clocking entity if one is specified
    (Just clockDir) <- getConfig "HDL_CLOCKS"
    (Just entityD) <- getConfig "TOPLEVEL_ENTITIES"
    (Just configF) <- getConfig "ENTITY_CONFIG_SETTINGS"
    entityConstraints <- (liftIO . readConf) $ entityD </> entityName </> configF
    let maybeClockHdl = do
          clockFile <- (getConf "clock" entityConstraints) :: Maybe String
          return $ clockDir </> clockFile -<.> hdlExt

    -- fetch the global hdl files
    (Just globalHdlD) <- getConfig "GLOBAL_HDL"
    globalHdlFs <- getDirectoryFiles "" [globalHdlD </> "*" <.> hdlExt]

    (Just vmPrefix) <- getConfig "VM_ROOT"

    let hdlFs = clashHdlFs ++ maybeToList maybeClockHdl ++ globalHdlFs

    writeFileLines prjF [ hdlName hdl ++ " work " ++ (vmPrefix </> hdlF) | hdlF <- hdlFs]

  (xilinxD </> "*.bit") %> \ bitF -> do
    need [bitF -<.> "prj",
          bitF -<.> "ucf"]

    let entityName = takeBaseName bitF
    let entityHdlName = entityName

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
    let xstOptLines' = reverse (optPre ++ ["\"-top " ++ entityHdlName ++ "\";"] ++ optSuf)
    -- this is ugly and I hate it but apparently this option isn't allowed on the spartan 6.
    let xstOptLines'' = filter (not . isInfixOf "-verilog2001") xstOptLines'

    writeFileLines (workD </> takeFileName xstOptF) xstOptLines''

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

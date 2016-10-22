module Make.Xilinx (xilinxRules) where

import Data.List
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import System.Posix.Escape

import Make.Config
import Make.Utils
import Make.Vagrant

xilinxRules = do
  (Just xilinxD) <- liftIO $ getConfigIO "XILINX_OUT"

  (xilinxD </> "*.prj") %> \ prjF -> do
    let entityName = takeBaseName prjF

    (Just clashOutD) <- getConfig "CLASH_OUT"
    let clashVhdlD = clashOutD </> entityName

    need [clashVhdlD </> "ndp_topentity.vhdl"]
    vhdlFs <- getDirectoryFiles "" [clashVhdlD  </> "*" <.> "vhdl"]

    (Just vmPrefix) <- getConfig "VM_ROOT"

    writeFileLines prjF ["vhdl \"" ++ (vmPrefix </> vhdlF) ++ "\"" | vhdlF <- vhdlFs]

  (xilinxD </> "*.bit") %> \ bitF -> do
    need [bitF -<.> "prj"]

    let workD = dropExtension bitF
    () <- cmd "mkdir -p" workD

    (Just vmPrefix) <- getConfig "VM_ROOT"
    (Just xilinxPart) <- getConfig "XILINX_PART"

    (Just settingsF) <- getConfig "XILINX_SETTINGS"

    (Just xstOptF) <- getConfig "XST_OPT"
    (Just xflowFastF) <- getConfig "XFLOW_FAST"
    (Just bitgenOptF) <- getConfig "XFLOW_BITGEN_OPT"

--     () <- cmd "cp -f" xstOptF (workD ++ "/")
    () <- cmd "cp -f" xflowFastF (workD ++ "/")
    () <- cmd "cp -f" bitgenOptF (workD ++ "/")

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

      cmd Shell "vagrant ssh -c" [sshCmd]

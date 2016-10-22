module Make.Xilinx (xilinxRules) where

import Data.List
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath

import Make.Config
import Make.Utils

xilinxRules = do
  (Just xilinxD) <- liftIO $ getConfigIO "XILINX_OUT"

  (xilinxD </> "*/project.prj") %> \ prjF -> do
    let workD = takeDirectory prjF
    let entityName = withPath (pure . last) $ workD
    (Just clashOutD) <- getConfig "CLASH_OUT"
    let clashVhdlD = clashOutD </> entityName

    need [clashVhdlD </> "ndp_topentity.vhdl"]
    vhdlFs <- getDirectoryFiles "" [clashVhdlD  </> "*" <.> "vhdl"]

    (Just vmPrefix) <- getConfig "VM_ROOT"

    writeFileLines prjF ["vhdl work " ++ (vmPrefix </> vhdlF) | vhdlF <- vhdlFs]

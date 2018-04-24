module Make.Shortcuts (shortcutRules) where

import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath

import Make.Command
import Make.Config
import Make.Vagrant

shortcutRules :: Rules ()
shortcutRules = do
  installCommandTree $ commandGroup ":" [fpgaCommands, clashCommands]

fpgaCommands :: CommandTree
fpgaCommands = commandGroup "fpga:" [mkCommand "reset:" resetCmd
                                    ,mkCommand "build:" buildCmd
                                    ,mkCommand "load:" loadCmd
                                    ,mkCommand "burn:" burnCmd]

clashCommands :: CommandTree
clashCommands = mkCommand "clash:" buildClashCmd

resetCmd :: String -> Action ()
resetCmd _ = do
  (Just fpgaProg) <- getConfig "FPGAPROG"
  withVagrant $ vagrantSSH ["sudo", fpgaProg, "-vr"]

buildCmd :: String -> Action ()
buildCmd project = do
  (Just container) <- getConfig "XILINX_OUT"
  need [container </> project -<.> "bit"]

loadCmd :: String -> Action ()
loadCmd project = do
  (Just container) <- getConfig "XILINX_OUT"
  (Just vmContainer) <- getConfig "VM_XILINX_OUT"
  (Just fpgaProg) <- getConfig "FPGAPROG"

  need [container </> project -<.> "bit"]

  let vagrantBitfile = vmContainer </> project -<.> "bit"

  withVagrant $ vagrantSSH ["sudo", fpgaProg, "-vf", vagrantBitfile]

burnCmd :: String -> Action ()
burnCmd project = do
  (Just container) <- getConfig "XILINX_OUT"
  (Just vmContainer) <- getConfig "VM_XILINX_OUT"
  (Just fpgaProg) <- getConfig "FPGAPROG"
  (Just burner) <- getConfig "FPGA_BURNER"

  need [container </> project -<.> "bit"]

  let vagrantBitfile = vmContainer </> project -<.> "bit"
  let burnCmdline = ["sudo", fpgaProg,
                 "-vf", vagrantBitfile,
                 "-b", burner,
                 "-sa", "-r"]

  withVagrant $ vagrantSSH burnCmdline

buildClashCmd :: String -> Action ()
buildClashCmd project = do
  buildDir <- maybeConfig "BUILD" "build"
  clashOut <- maybeConfig "CLASH_OUT" (buildDir </> "clash")
  need [clashOut </> project </> project -<.> "vhdl"]

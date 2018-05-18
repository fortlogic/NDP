module Make.Shortcuts (shortcutRules) where

import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath

import Make.Command
import Make.Config
import Make.HDL
import Make.Vagrant

shortcutRules :: Rules ()
shortcutRules = do
  installCommandTree $ commandGroup ":" [fpgaCommands, clashCommands, testbenchCommands]

fpgaCommands :: CommandTree
fpgaCommands = commandGroup "fpga:" [mkCommand "reset:" resetCmd
                                    ,mkCommand "build:" buildCmd
                                    ,mkCommand "load:" loadCmd
                                    ,mkCommand "burn:" burnCmd
                                    ,mkCommand "stage:" stageCmd]

clashCommands :: CommandTree
clashCommands = commandGroup "clash:" [ mkCommand "vhdl:" (buildClashCmd VHDL)
                                      , mkCommand "verilog:" (buildClashCmd Verilog)]

testbenchCommands :: CommandTree
testbenchCommands = commandGroup "testbench:" [ mkCommand "vhdl:" (runHDLTestbench VHDL)]

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

stageCmd :: String -> Action ()
stageCmd project = do
  (Just container) <- getConfig "XILINX_OUT"
  need [ container </> project -<.> "prj"
       , container </> project -<.> "ucf"]

buildClashCmd :: HDL -> String -> Action ()
buildClashCmd hdl project = do
  buildDir <- maybeConfig "BUILD" "build"
  clashOut <- maybeConfig "CLASH_OUT" (buildDir </> "clash")
  need [ clashOut  </> hdlName hdl </> project </> "manifest" <.> "txt" ]

runHDLTestbench :: HDL -> String -> Action ()
runHDLTestbench hdl project = do
  putNormal "This feature doesn't exist yet."

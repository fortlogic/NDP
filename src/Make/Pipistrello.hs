module Make.Pipistrello (pipistrelloRules) where

import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath

import Make.Command

pipistrelloRules = do
  installCommandTree fpgaCommands

fpgaCommands = commandGroup "fpga:" [mkCommand "reset:" resetCmd
                                    ,mkCommand "build:" buildCmd]

resetCmd :: String -> Action ()
resetCmd _ = do
  putNormal "RESET"

buildCmd :: String -> Action ()
buildCmd bitfile = do
  (Just container) <- getConfig "XILINX_OUT"
  need [container </> bitfile]

loadCmd :: String -> Action ()
loadCmd = undefined

burnCmd :: String -> Action ()
burnCmd = undefined

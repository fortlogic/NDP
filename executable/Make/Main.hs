module Main where

import Development.Shake

import Make.Clash
import Make.Config
import Make.GHDL
import Make.Oracles
import Make.Shortcuts
import Make.TileROM
import Make.Xilinx.Constraints
import Make.Xilinx.XFlow

main :: IO ()
main = shakeArgs shakeOptions $ do
  installOracles
  setupConfig "NDP.config"

  clashRules
  ghdlRules
  shortcutRules
  tileROMRules
  xflowRules
  ucfRules

module Main where

import Development.Shake
import Options.Applicative
-- import System.Environment

import Make.Clash
import Make.Command
import Make.Config
import Make.GHDL
import Make.Oracles
import Make.Shortcuts
import Make.TileROM
import Make.Xilinx.Constraints
import Make.Xilinx.XFlow

main :: IO ()
main = execParser (info (commands <**> helper) mempty) >>= (putStrLn . show)

-- customise the command line arguments shakeMain sees using `withArgs`.
shakeMain :: IO ()
shakeMain = shakeArgs shakeOptions $ do
  installOracles
  setupConfig "NDP.config"

  clashRules
  ghdlRules
  shortcutRules
  tileROMRules
  xflowRules
  ucfRules

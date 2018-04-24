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
-- import qualified Tests.Main as T



main :: IO ()
main = shakeArgs shakeOptions $ do
  installOracles
  setupConfig "NDP.config"

  phony "setup" $ do
    cmd "stack build clash-ghc"

  phony "clean" $ do
    cmd "rm -rvf build"

  -- phony "test" $ do
  --   liftIO $ do
  --     (_:args) <- getArgs
  --     withArgs args T.main

  clashRules
  ghdlRules
  shortcutRules
  tileROMRules
  xflowRules
  ucfRules

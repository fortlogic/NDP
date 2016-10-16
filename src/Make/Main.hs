module Main where

import Data.Maybe
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import System.Environment


import Make.Clash
import Make.Config
import Make.GHDL
import Make.Oracles
import Make.TileROM
import Make.Vagrant
import qualified Tests.Main as T



main :: IO ()
main = shakeArgs shakeOptions $ do
  installOracles
  setupConfig "NDP.config"

  phony "setup" $ do
    cmd "stack build clash-ghc"

  phony "clean" $ do
    cmd "rm -rvf build"

  phony "test" $ do
    liftIO $ do
      (_:args) <- getArgs
      withArgs args T.main

  clashRules
  tileROMRules
  ghdlRules

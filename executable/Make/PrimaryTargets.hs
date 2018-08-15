module Make.PrimaryTargets (primaryRules) where

import Development.Shake

import Make.Config

primaryRules :: Rules ()
primaryRules = do
  phony "setup" $ do
    () <- cmd "stack setup"
    () <- cmd "stack build"
    () <- cmd "stack build clash-ghc"
    return ()

  phony "clean" $ do
    buildD <- getBuildDir
    cmd "rm -rvf" buildD

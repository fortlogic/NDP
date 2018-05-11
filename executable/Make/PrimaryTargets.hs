module Make.PrimaryTargets (primaryRules) where

import Development.Shake
import Development.Shake.Config

primaryRules :: Rules ()
primaryRules = do
  phony "setup" $ do
    () <- cmd "stack setup"
    () <- cmd "stack build"
    () <- cmd "stack build clash-ghc"
    return ()

  phony "clean" $ do
    (Just buildD) <- getConfig "BUILD"
    cmd "rm -rvf" buildD

  -- phony "archive" $ do
  --   (Stdout rawDate) <- cmd "date -u +UTC-%Y-%m-%d-%H%M%S"
  --   let date = filter (/= '\n') rawDate

  --   (Just buildD) <- getConfig "BUILD"
  --   (Just archiveD) <- getConfig "ARCHIVE"

  --   withTempFile $ \ tmpF -> do
  --     () <- cmd "tar -cvf" tmpF buildD
  --     (Stdout bzip) <- cmd "bzip2 -cz" tmpF
  --     writeFile' (archiveD </> date <.> "bz2") bzip

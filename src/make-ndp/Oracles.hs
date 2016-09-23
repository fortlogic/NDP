{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Oracles (installOracles,
                ClashVersion (ClashVersion),
                clashVersionAddOracle,
                clashVersionIO) where

import Data.List
import Development.Shake
import Development.Shake.Classes

installOracles :: Rules ()
installOracles = do
  clashVersionAddOracle
  return ()

newtype ClashVersion = ClashVersion ()
                     deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

clashVersionAddOracle :: Rules (ClashVersion -> Action String)
clashVersionAddOracle = addOracle $ \ (ClashVersion _) ->
                                   liftIO clashVersionIO

clashVersionIO :: IO String
clashVersionIO = delete <$> pure '\n' <*> (fromStdout <$> cmd "clash --numeric-version")

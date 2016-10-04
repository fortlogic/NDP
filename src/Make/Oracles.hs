{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Make.Oracles (installOracles,
                     ClashVersion (ClashVersion),
                     clashVersionAddOracle,
                     clashVersionIO,
                     OSName,
                     osNameAddOracle,
                     osNameIO) where

import Data.List
import Development.Shake
import Development.Shake.Classes

installOracles :: Rules ()
installOracles = do
  clashVersionAddOracle
  osNameAddOracle
  return ()

newtype ClashVersion = ClashVersion ()
                     deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

clashVersionAddOracle :: Rules (ClashVersion -> Action String)
clashVersionAddOracle = addOracle $ \ (ClashVersion _) ->
                                   liftIO clashVersionIO

clashVersionIO :: IO String
clashVersionIO = delete <$> pure '\n' <*> (fromStdout <$> cmd "clash --numeric-version")

newtype OSName = OSName ()
               deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

osNameAddOracle :: Rules (OSName -> Action String)
osNameAddOracle = addOracle $ \ (OSName _) ->
                                liftIO osNameIO

osNameIO :: IO String
osNameIO = delete '\n' <$> (fromStdout <$> cmd "uname")

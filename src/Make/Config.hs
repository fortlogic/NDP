{-# LANGUAGE TemplateHaskell #-}
module Make.Config (setupConfig,
                    maybeConfig,
                    configFlag,
                    configFlag2,
                    getConfigIO,
                    maybeConfigIO) where

import Control.Concurrent.MVar
import Data.Global
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Development.Shake
import Development.Shake.Config

import Make.Oracles

declareEmptyMVar "configV" [t| (H.HashMap String String) |]

getConfigIO :: String -> IO (Maybe String)
getConfigIO key = H.lookup key <$> readMVar configV

initialConfig :: Rules [(String, String)]
initialConfig = do
  clashVer <- liftIO clashVersionIO
  platform <- liftIO osPlatformIO
  arch <- liftIO cpuArchitectureIO
  return [("CLASH_VER", clashVer),
          ("ARCH", arch),
          ("PLATFORM", platform)]

setupConfig :: FilePath -> Rules ()
setupConfig configFile = do
  inits <- initialConfig
  config <- liftIO $ readConfigFileWithEnv inits configFile
  liftIO $ putMVar configV config
  usingConfig config

maybeConfig :: String -> String -> Action String
maybeConfig key fallback = fromMaybe <$> return fallback <*> getConfig key

maybeConfigIO :: String -> String -> IO String
maybeConfigIO key fallback = fromMaybe <$> return fallback <*> getConfigIO key

configFlag :: String -> String -> Action String
configFlag flagName configKey = do
  maybeVal <- getConfig configKey
  return $ case maybeVal of
    Nothing -> ""
    Just val -> flagName ++ val


configFlag2 :: String -> String -> Action [String]
configFlag2 flagName configKey = do
  maybeVal <- getConfig configKey
  return $ case maybeVal of
    Nothing -> []
    Just val -> [flagName, val]

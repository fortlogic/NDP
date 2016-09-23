module Config (setupConfig,
               maybeConfig,
               configFlag,
               configFlag2) where

import Data.Maybe
import Development.Shake
import Development.Shake.Config

import Oracles

initialConfig :: Rules [(String, String)]
initialConfig = do
  clashVer <- liftIO clashVersionIO
  return [("CLASH_VER", clashVer)]

setupConfig :: FilePath -> Rules ()
setupConfig configFile = do
  inits <- initialConfig
  config <- liftIO $ readConfigFileWithEnv inits configFile
  usingConfig config

maybeConfig :: String -> String -> Action String
maybeConfig key fallback = fromMaybe <$> return fallback <*> getConfig key

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

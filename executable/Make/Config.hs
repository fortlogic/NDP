{-# LANGUAGE TemplateHaskell #-}
module Make.Config (setupConfig,
                    maybeConfig,
                    configFlag,
                    configFlag2,
                    getConfigIO,
                    maybeConfigIO) where

import Control.Concurrent.MVar
import Data.Global
import Data.List (intercalate)
import qualified Data.HashMap.Strict as H
-- import Data.Maybe
import Development.Shake
import Development.Shake.Config
import qualified System.Environment as E

import Make.Oracles
import Resources.ROM.Tiles

declareEmptyMVar "configV" [t| (H.HashMap String String) |]

getConfigIO :: String -> IO (Maybe String)
getConfigIO key = H.lookup key <$> readMVar configV

initialConfig :: Rules [(String, String)]
initialConfig = do
  clashVer <- liftIO clashVersionIO
  platform <- liftIO osPlatformIO
  arch <- liftIO cpuArchitectureIO
  xilinxRoot <- liftIO $ E.getEnv "XILINXROOT"
  return [("CLASH_VER", clashVer),
          ("ARCH", arch),
          ("PLATFORM", platform),
          ("XILINX", xilinxRoot)]

setupConfig :: FilePath -> Rules ()
setupConfig configFile = do
  inits <- initialConfig
  config <- liftIO $ readConfigFileWithEnv inits configFile
  liftIO $ putMVar configV config
  usingConfig config

missingMsg :: String -> String -> String
missingMsg var fallback = intercalate " " [notFoundMsg, fallbackMsg]
  where notFoundMsg = concat ["Config variable '", var, "' not found."]
        fallbackMsg = concat ["Using default value of '", fallback, "'."]

maybeConfig :: String -> String -> Action String
maybeConfig key fallback = do
  maybeVal <- getConfig key
  case maybeVal of
    Nothing -> do
      -- complain if the key isn't present.
      putNormal $ missingMsg key fallback
      return fallback
    Just val -> return val

maybeConfigIO :: String -> String -> IO String
maybeConfigIO key fallback = do
  maybeVal <- getConfigIO key
  case maybeVal of
    Nothing -> do
      -- complain if the key isn't present.
      putStrLn $ missingMsg key fallback
      return fallback
    Just val -> return val

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

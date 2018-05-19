{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Make.Config (setupConfig
                   , maybeConfig
                   , readConfig
                   , maybeReadConfig
                   , configFlag
                   , configFlag2
                   , getConfigIO
                   , maybeConfigIO
                   , readConfigIO
                   , maybeReadConfigIO

                   , getBuildDir
                   , getClashDir
                   , getGHDLDir
                   , getXilinxDir
                   , getVMXilinxDir ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Global
import Data.List (intercalate)
import Data.Maybe
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import Text.Read
import qualified Data.HashMap.Strict as H
import qualified System.Environment as E

import Make.Oracles

declareEmptyMVar "configV" [t| (H.HashMap String String) |]

getConfigIO :: String -> IO (Maybe String)
getConfigIO key = H.lookup key <$> readMVar configV

initialConfig :: Rules [(String, String)]
initialConfig = do
  clashVer <- liftIO clashVersionIO
  platform <- liftIO osPlatformIO
  arch <- liftIO cpuArchitectureIO
  xilinxRoot <- liftIO $ E.lookupEnv "XILINXROOT"
  (return . catMaybes) [ Just ("CLASH_VER", clashVer)
                       , Just ("ARCH", arch)
                       , Just ("PLATFORM", platform)
                       , ("XILINX",) <$> xilinxRoot ]

setupConfig :: FilePath -> Rules ()
setupConfig configFile = do
  want [configFile]
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
  getConfig key >>= \case
    Nothing -> do
      -- complain if the key isn't present.
      putNormal $ missingMsg key fallback
      return fallback
    Just val -> return val

readConfig :: Read a => String -> Action (Maybe a)
readConfig key = getConfig key >>= \case
  Nothing -> return Nothing
  (Just str) -> return (readMaybe str)

maybeReadConfig :: Read a => String -> a -> Action a
maybeReadConfig key fallback = (fromMaybe fallback) <$> readConfig key

maybeConfigIO :: String -> String -> IO String
maybeConfigIO key fallback = do
  getConfigIO key >>= \case
    Nothing -> do
      -- complain if the key isn't present.
      putStrLn $ missingMsg key fallback
      return fallback
    Just val -> return val

readConfigIO :: Read a => String -> IO (Maybe a)
readConfigIO key = getConfigIO key >>= \case
  Nothing -> return Nothing
  (Just str) -> return (readMaybe str)

maybeReadConfigIO :: Read a => String -> a -> IO a
maybeReadConfigIO key fallback = (fromMaybe fallback) <$> readConfigIO key

configFlag :: String -> String -> Action String
configFlag flagName configKey = do
  getConfig configKey >>= \case
    Nothing -> return ""
    Just val -> return $ flagName ++ val


configFlag2 :: String -> String -> Action [String]
configFlag2 flagName configKey = do
  maybeVal <- getConfig configKey
  return $ case maybeVal of
    Nothing -> []
    Just val -> [flagName, val]

-- useful shorthands

-- TODO: make shake give out a warning for all of these anytime the config
-- variable can't be found and the default is used instead. In fact maybe make
-- that a thing that the above things do as well? If that involves overriding
-- stuff in Development.Shake.Config then have this export everything it does,
-- and make sure everything else includes this module instead.

getBuildDir :: MonadIO m => m String
getBuildDir = liftIO $ maybeConfigIO "BUILD_OUT" "build"

getClashDir :: MonadIO m => m String
getClashDir = getBuildDir >>= fetch
  where fetch buildDir = liftIO $ maybeConfigIO "CLASH_OUT" (buildDir </> "clash")

getGHDLDir :: MonadIO m => m String
getGHDLDir = getBuildDir >>= fetch
  where fetch buildDir = liftIO $ maybeConfigIO "GHDL_OUT" (buildDir </> "clash")

getXilinxDir :: MonadIO m => m (Maybe String)
getXilinxDir = liftIO $ getConfigIO "XILINX_OUT"

getVMXilinxDir :: MonadIO m => m String
getVMXilinxDir = liftIO $ maybeConfigIO "VM_XILINX_OUT" "/xilinx"

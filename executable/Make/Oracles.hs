{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Make.Oracles (installOracles,
                     ClashVersion (..),
                     clashVersionAddOracle,
                     clashVersionIO,
                     OSPlatform (..),
                     osPlatformAddOracle,
                     osPlatformIO,
                     CPUArchitecture,
                     cpuArchitectureAddOracle,
                     cpuArchitectureIO) where

import Data.List
import Development.Shake
import Development.Shake.Classes

import Make.Vagrant

installOracles :: Rules ()
installOracles = do
  _ <- clashVersionAddOracle
  _ <- osPlatformAddOracle
  _ <- vagrantStatusAddOracle
  _ <- cpuArchitectureAddOracle
  return ()

newtype ClashVersion = ClashVersion ()
                     deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

clashVersionAddOracle :: Rules (ClashVersion -> Action String)
clashVersionAddOracle = addOracle $ \ (ClashVersion _) ->
                                   liftIO clashVersionIO

clashVersionIO :: IO String
clashVersionIO = delete <$> pure '\n' <*> (fromStdout <$> cmd "clash --numeric-version")

newtype OSPlatform = OSPlatform ()
               deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

osPlatformAddOracle :: Rules (OSPlatform -> Action String)
osPlatformAddOracle = addOracle $ \ (OSPlatform _) ->
                                liftIO osPlatformIO

osPlatformIO :: IO String
osPlatformIO = delete '\n' <$> (fromStdout <$> cmd "uname")

newtype CPUArchitecture = CPUArchitecture ()
               deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

cpuArchitectureAddOracle :: Rules (CPUArchitecture -> Action String)
cpuArchitectureAddOracle = addOracle $ \ (CPUArchitecture _) ->
                                liftIO cpuArchitectureIO

cpuArchitectureIO :: IO String
cpuArchitectureIO = delete '\n' <$> (fromStdout <$> cmd "arch")


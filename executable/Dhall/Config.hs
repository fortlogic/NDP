{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Dhall.Config () where

import Dhall

import Make.HDL

data NDPConfiguration = NDPConfig
  { clash :: ClashConfiguration
  , ghdl :: GHDLConfiguration
  , rom :: ROMConfiguration
  , fpga :: FPGAConfiguration
  }
  deriving (Generic, Show)

instance Interpret NDPConfiguration

data ClashConfiguration = ClashConfig
  { componentsDir :: FilePath
  , defaultHDL :: HDL
  , hardwareLibraryDir :: FilePath
  , hdlOutDir :: FilePath
  , ghcArtefactDir :: FilePath
  , primitivesDir :: FilePath
  , topLevelClashFile :: FilePath
  , componentConfigFile :: FilePath
  }
  deriving (Generic, Show)

instance Interpret ClashConfiguration

data GHDLConfiguration = GHDLConfig
  { artefactsDir :: FilePath }
  deriving (Generic, Show)

instance Interpret GHDLConfiguration

data ROMConfiguration = ROMConfig
  { tileDir :: FilePath
  }
  deriving (Generic, Show)

instance Interpret ROMConfiguration

data FPGAConfiguration = FPGAConfig
  { vm :: Maybe VMConfiguration
  , part :: String
  , fpgaProg :: FilePath
  , burnBitfile :: FilePath
  , synthToolchain :: HDLSynthesisConfiguration
  }
  deriving (Generic, Show)

instance Interpret FPGAConfiguration

data HDLSynthesisConfiguration = XilinxConfig
  { shellProfile :: FilePath -- settings64.sh
  , xstOptFile :: FilePath
  , xflowRuntimeFile :: FilePath
  , bitgenOptFile :: FilePath
  }
  deriving (Generic, Show)

instance Interpret HDLSynthesisConfiguration

data VMConfiguration = VMConfig
  { projectRootDir :: FilePath
  }
  deriving (Generic, Show)

instance Interpret VMConfiguration

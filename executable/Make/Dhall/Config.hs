{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Make.Dhall.Config () where

import Dhall

import Make.HDL

data NDPConfiguration = NDPConfig
  { clash :: ClashConfiguration
  , ghdl :: GHDLConfiguration
  , rom :: ROMConfiguration
  , fpga :: FPGAConfiguration
  }
  deriving (Generic, Show)

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

data GHDLConfiguration = GHDLConfig
  { artefactsDir :: FilePath }
  deriving (Generic, Show)

data ROMConfiguration = ROMConfig
  { tileDir :: FilePath
  }
  deriving (Generic, Show)

data FPGAConfiguration = FPGAConfig
  { vm :: Maybe VMConfiguration
  , part :: String
  , fpgaProg :: FilePath
  , burnBitfile :: FilePath
  , synthToolchain :: HDLSynthesisConfiguration
  }
  deriving (Generic, Show)

data HDLSynthesisConfiguration = XilinxConfig
  { shellProfile :: FilePath -- settings64.sh
  , xstOptFile :: FilePath
  , xflowRuntimeFile :: FilePath
  , bitgenOptFile :: FilePath
  }
  deriving (Generic, Show)

data VMConfiguration = VMConfig
  { projectRootDir :: FilePath
  }
  deriving (Generic, Show)

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies#-}
module Make.Dhall.Hardware ( Project (..)
                           , Target (..)
                           , HDLIncludes (..)
                           , HardwareProjects (..)
                           , hardwareProjectsAddOracle
                           ) where

import qualified Data.Text as T
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Config
import Dhall

data Project = Project
  { root :: String
  , name :: String
  , targets :: [Target]
  } deriving (Generic, Show, Eq, Interpret, Hashable, Binary, NFData)

data Target = Target
  { target :: String
  , entities :: [String]
  , source :: String
  , hdl :: [HDLIncludes]
  } deriving (Generic, Show, Eq, Interpret, Hashable, Binary, NFData)

data HDLIncludes = HDLIncludes
  { language :: String
  , files :: [String]
  } deriving (Generic, Show, Eq, Interpret, Hashable, Binary, NFData)

project :: Type Project
project = auto

newtype HardwareProjects = HardwareProjects ()
                      deriving ( Generic, Show, Eq
                               , Typeable, Hashable, Binary, NFData)
type instance RuleResult HardwareProjects = [Project]

hardwareProjectsAddOracle :: Rules (HardwareProjects -> Action [Project])
hardwareProjectsAddOracle = addOracle $ \ (HardwareProjects _) -> do
  (Just projectsPath) <- getConfig "PROJECTS_FILE"
  liftIO $ input (list project) ("./" `T.append` T.pack projectsPath)

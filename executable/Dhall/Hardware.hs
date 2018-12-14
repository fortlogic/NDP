{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies#-}
module Dhall.Hardware ( Project (..)
                      , Target (..)
                      , HDLIncludes (..)
                      , HardwareProjects (..)
                      , hardwareProjectsAddOracle
                      , projectNamed
                      , projectRoot'
                      , targetNamed
                      ) where

import Data.List
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Config
import Development.Shake.FilePath
import Dhall

data Project = Project
  { projectRoot :: String
  , projectName :: String
  , projectTargets :: [Target]
  } deriving (Generic, Show, Eq, Interpret, Hashable, Binary, NFData)

data Target = Target
  { targetName :: String
  , targetEntities :: [String]
  , targetSource :: FilePath
  , targetHDL :: [HDLIncludes]
  } deriving (Generic, Show, Eq, Interpret, Hashable, Binary, NFData)

data HDLIncludes = HDLIncludes
  { hdlLanguage :: String
  , hdlFiles :: [FilePath]
  } deriving (Generic, Show, Eq, Interpret, Hashable, Binary, NFData)

project :: Type Project
project = record (Project <$> rootF <*> nameF <*> targetsF)
  where rootF = field "root" string
        nameF = field "name" string
        targetsF = field "targets" (list target)

target :: Type Target
target = record (Target <$> nameF <*> entitiesF <*> sourceF <*> hdlF)
  where nameF = field "target" string
        entitiesF = field "entities" (list string)
        sourceF = field "source" string
        hdlF = field "hdl" (list hdlIncludes)

hdlIncludes :: Type HDLIncludes
hdlIncludes = record (HDLIncludes <$> languageF <*> filesF)
  where languageF = field "language" string
        filesF = field "files" (list string)

newtype HardwareProjects = HardwareProjects ()
                      deriving ( Generic, Show, Eq
                               , Typeable, Hashable, Binary, NFData)
type instance RuleResult HardwareProjects = [Project]

hardwareProjectsAddOracle :: Rules (HardwareProjects -> Action [Project])
hardwareProjectsAddOracle = addOracle $ \ (HardwareProjects _) -> do
  (Just projectsPath) <- getConfig "PROJECTS_FILE"
  liftIO $ input (list project) ("./" `T.append` T.pack projectsPath)

projectNamed :: String -> [Project] -> Maybe Project
projectNamed n = find ((==n) . projectName)

projectRoot' :: Project -> Action FilePath
projectRoot' prj = do
  (Just prjsRoot) <- getConfig "PROJECTS_ROOT"
  return (prjsRoot </> projectRoot prj)

targetNamed :: String -> Project -> Maybe Target
targetNamed n = find ((==n) . targetName) . projectTargets

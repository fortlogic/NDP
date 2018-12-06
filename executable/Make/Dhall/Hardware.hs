{-# LANGUAGE DeriveGeneric #-}
module Make.Dhall.Hardware where

import Dhall

data Project = Project { root :: String
                       , name :: String
                       , targets :: [Target]
                       } deriving (Generic, Show)

instance Interpret Project

data Target = Target { target :: String
                     , entities :: [String]
                     , source :: String
                     , hdl :: [HDLIncludes]
                     } deriving (Generic, Show)

instance Interpret Target

data HDLIncludes = HDLIncludes { language :: String
                               , files :: [String]
                               } deriving (Generic, Show)

instance Interpret HDLIncludes

project :: Type Project
project = auto

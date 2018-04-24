{-# LANGUAGE OverloadedStrings #-}
module Make.Xilinx.Constraints ( ucfRules
                               , Constraints (..)
                               , NetConstraint (..)
                               , NetParameter (..)) where

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as B
import Data.Conf
import Data.List as L
import Data.Monoid
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath

import Make.Config
import Resources.Constraints

-- I want a shorter name for this
utf8 = stringUtf8

ucfRules = do
  (Just xilinxD) <- liftIO $ getConfigIO "XILINX_OUT"

  (xilinxD </> "*.ucf") %> \ ucF -> do
    let entityName = takeBaseName ucF

    -- read config vars
    (Just masterConstraintsF) <- getConfig "FPGA_CONSTRAINTS"
    (Just entityD) <- getConfig "TOPLEVEL_ENTITIES"
    (Just mainClashNameF) <- getConfig "TOPLEVEL_HS_FILE"
    (Just constraintsF) <- getConfig "ENTITY_CONFIG_SETTINGS"

    need [masterConstraintsF,
          entityD </> entityName </> constraintsF]

    -- read from constraint files
    masterConstraints <- liftIO $ readConf masterConstraintsF
    entityConstraints <- (liftIO . readConf) $ entityD </> entityName </> constraintsF

    -- determine constraints worth fetching
    let (Just constraintTags) = (getConf "constraints" entityConstraints) :: Maybe [String]

    constraintBuilders <- (flip mapM) constraintTags $ \ tag -> do
      let (Just constraints) = (getConf tag masterConstraints) :: Maybe Constraints
      return $ renderConstraints constraints

    (liftIO . B.writeFile ucF . B.concat . map toLazyByteString) constraintBuilders

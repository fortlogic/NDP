{-# LANGUAGE OverloadedStrings #-}
module Make.Xilinx.Constraints ( ucfRules
                               , Constraints (..)
                               , NetConstraint (..)
                               , NetParameter (..)) where

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as B
import Data.Conf
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath

import Make.Config
import Constraints

ucfRules :: Rules ()
ucfRules = do
  (Just xilinxD) <- getXilinxDir

  (xilinxD </> "*.ucf") %> \ ucF -> do
    let entityName = takeBaseName ucF

    -- read config vars
    (Just masterConstraintsF) <- getConfig "FPGA_CONSTRAINTS"
    (Just entityD) <- getConfig "HDL_PROJECTS"
    (Just constraintsF) <- getConfig "HDL_PROJECT_CONFIG_FILE"

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

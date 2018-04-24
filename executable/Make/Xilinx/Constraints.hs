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
utf8 :: String -> Builder
utf8 = stringUtf8

ucfRules :: Rules ()
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

renderConstraints :: Constraints -> Builder
renderConstraints cs = renderLines (rawB ++ netB)
  where rawB = map stringUtf8 (rawConstraints cs)
        netB = (netConstraints cs) >>= renderNet

renderLines :: [Builder] -> Builder
renderLines lns = mconcat [ ln <> stringUtf8 ";\n" | ln <- lns ]

renderNet :: NetConstraint -> [Builder]
renderNet (SingleNet name loc attrs) = [renderPrimNet (name <> "(0)") (locP : attrs)]
  where locP = NetKV "LOC" loc
renderNet (SingleNetLocless name attrs) = [renderPrimNet (name <> "(0)") attrs]
renderNet (BusNet name locs attrs) = zipWith mkSingle locs [0..]
  where mkSingle loc idx = renderPrimNet (mkName idx) (NetKV "LOC" loc : attrs)
        mkName idx = name ++ "(" ++ show idx ++ ")"

renderPrimNet :: String -> [NetParameter] -> Builder
renderPrimNet name attrs = mconcat $ intersperse spc ["NET", netName, renderAttribs attrs]
  where netName = stringUtf8 name
        spc = charUtf8 ' '

renderNetParameter :: NetParameter -> Builder
renderNetParameter (NetFlag f)        = stringUtf8 f
renderNetParameter (NetKV l r) = utf8 l <> utf8 " = " <> utf8 r

renderAttribs :: [NetParameter] -> Builder
renderAttribs attr = mconcat $ intersperse sep $ map renderNetParameter attr
  where sep = stringUtf8 " | "

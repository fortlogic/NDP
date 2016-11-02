module Make.Xilinx.Constraints (ucfRules) where

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as B
import Data.Conf
import Data.List
import Data.Monoid
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath

import Make.Config

ucfRules = do
  (Just xilinxD) <- liftIO $ getConfigIO "XILINX_OUT"

  (xilinxD </> "*.ucf") %> \ ucF -> do
    let entityName = takeBaseName ucF

    -- read config vars
    (Just masterConstraintsF) <- getConfig "FPGA_CONSTRAINTS"
    (Just entityD) <- getConfig "TOPLEVEL_ENTITIES"
    (Just mainClashNameF) <- getConfig "TOPLEVEL_HS_FILE"
    (Just constraintsF) <- getConfig "ENTITY_CONSTRAINTS"

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

data Constraints = Constraints {
  rawConstraints :: [String],
  netConstraints :: [(String, [(String, String)])]
  } deriving (Read, Show)

renderConstraints :: Constraints -> Builder
renderConstraints cs = renderLines (rawB ++ netB)
  where rawB = map stringUtf8 (rawConstraints cs)
        netB = map renderNet (netConstraints cs)

renderLines :: [Builder] -> Builder
renderLines lns = mconcat [ ln <> stringUtf8 ";\n" | ln <- lns ]

renderNet :: (String, [(String, String)]) -> Builder
renderNet (name, attrs) = net <> spc <> netName <> spc <> renderAttribs attrs
  where net = stringUtf8 "NET"
        netName = stringUtf8 name
        spc = charUtf8 ' '

renderAttribs :: [(String, String)] -> Builder
renderAttribs attr = mconcat $ intersperse sep [ render l r | (l, r) <- attr]
  where render l r = stringUtf8 l <> stringUtf8 " = " <> stringUtf8 r
        sep = stringUtf8 " | "

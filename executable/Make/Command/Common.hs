module Make.Command.Common where

import Options.Applicative

import Make.HDL

maybeP :: Parser a -> Parser (Maybe a)
maybeP f = (Just <$> f) <|> pure Nothing

data BuildTarget = BuildTarget
  { project :: String
  , target :: String
  } deriving (Show)

buildTargetP :: Parser BuildTarget
buildTargetP = BuildTarget <$> projectP <*> targetP
  where projectP = strArgument (metavar "PROJECT")
        targetP = strArgument (metavar "TARGET")

hdlP :: Parser HDL
hdlP = option (eitherReader parseHDL') ( metavar "HDL"
                                         <> long "hdl"
                                         <> short 'l'
                                         <> help "Use the specified HDL")

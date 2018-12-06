module Make.Command where

import Data.Semigroup ((<>))
import Development.Shake (Verbosity (..))
import Options.Applicative as Opt

import Make.HDL

data Options = Options
  { verbosity :: Verbosity
  , command :: Command
  } deriving ( Show )

data Command = VersionCommand VersionOptions
             | ClashCommand ClashOptions
             deriving (Show)

data VersionOptions = VersionOptions
  { machineReadable :: Bool
  } deriving (Show)

data ClashOptions = ClashOptions
  { hdl :: Maybe HDL
  } deriving (Show)

commandOptionP :: Parser Options
commandOptionP = Options <$> verbosityP <*> commandP

commandP :: Parser Command
commandP = printVersionP <|> commandsP
  where commandsP = hsubparser (clashCmd)
        clashCmd = Opt.command "clash"
                   (info (ClashCommand <$> clashOptionsP)
                     (progDesc "Compile Haskell to an HDL using Clash"))

printVersionP :: Parser Command
printVersionP = (VersionCommand (VersionOptions False) <$ humanFlag)
                <|> (VersionCommand (VersionOptions True) <$ machineFlag)
  where humanFlag   = flag' () ( long "version"
                                 <> help "Print version.")
        machineFlag = flag' () ( long "numeric-version"
                                 <> help "Print only version number" )

verbosityP :: Parser Verbosity
verbosityP = silentP <|> ((mkVerbosity . sum) <$> many (verboseP <|> quietP))
  where verboseP = flag'   1  ( long "verbose"
                                <> short 'v'
                                <> help "Print more (pass repeatedly for emphasis)")
        quietP   = flag' (-1) ( long "quiet"
                                <> short 'q'
                                <> help "Print less (pass repeatedly for emphasis)")

        silentP  = flag' Silent ( long "silent"
                                  <> short 's'
                                  <> help "Don't print anything")

mkVerbosity :: Int -> Verbosity
mkVerbosity (-2) = Silent
mkVerbosity (-1) = Quiet
mkVerbosity   0 = Normal
mkVerbosity   1 = Loud
mkVerbosity   2 = Chatty
mkVerbosity   3 = Diagnostic
mkVerbosity   n = if n > 0 then Diagnostic else Silent

hdlP :: Parser (Maybe HDL)
hdlP = parseHDL <$> strOption (long "hdl" <> short 'l' <> help "Use the specified HDL")

clashOptionsP :: Parser ClashOptions
clashOptionsP = ClashOptions <$> hdlP

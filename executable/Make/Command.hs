module Make.Command where

import Data.Functor
import Data.Semigroup ((<>))
import Development.Shake (Verbosity (..))
import Options.Applicative

data GeneralOptions = General
  { verbosity :: Verbosity
  } deriving ( Show )

data Command = PrintVersion
  { machineReadable :: Bool
  } deriving ( Show )

commandOptionP :: Parser (Command, GeneralOptions)
commandOptionP = (,) <$> commandP <*> generalOptionsP

commandP :: Parser Command
commandP = printVersionP

generalOptionsP :: Parser GeneralOptions
generalOptionsP = General <$> verbosityP

printVersionP :: Parser Command
printVersionP = (PrintVersion False <$ humanFlag)
                <|> (PrintVersion True <$ machineFlag)
  where humanFlag   = flag' () ( long "version"
                                 <> help "Print version.")
        machineFlag = flag' () ( long "numeric-version"
                                 <> help "Print only version number." )

verbosityP :: Parser Verbosity
verbosityP = silentP <|> ((mkVerbosity . sum) <$> many (verboseP <|> quietP))
  where verboseP = flag'   1  ( long "verbose"
                                <> short 'v'
                                <> help "Print more (pass repeatedly for emphasis).")
        quietP   = flag' (-1) ( long "quiet"
                                <> short 'q'
                                <> help "Print less (pass repeatedly for emphasis).")

        silentP  = flag' Silent ( long "silent"
                                  <> short 's'
                                  <> help "Don't print anything.")

mkVerbosity :: Int -> Verbosity
mkVerbosity (-2) = Silent
mkVerbosity (-1) = Quiet
mkVerbosity   0 = Normal
mkVerbosity   1 = Loud
mkVerbosity   2 = Chatty
mkVerbosity   3 = Diagnostic
mkVerbosity   n = if n > 0 then Diagnostic else Silent

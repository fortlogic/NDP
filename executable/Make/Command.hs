module Make.Command where

import Data.Semigroup ((<>))
import Development.Shake (Verbosity (..))
import Options.Applicative

commands :: Parser Verbosity
commands = verbFlag


verbFlag :: Parser Verbosity
verbFlag = silent <|> ((mkVerbosity . sum) <$> many (verbose <|> quiet))
  where verbose = flag' 1 ( long "verbose"
                            <> short 'v'
                            <> help "Print more (pass repeatedly for emphasis).")
        quiet = flag' (-1) ( long "quiet"
                           <> short 'q'
                           <> help "Print less (pass repeatedly for emphasis).")
        silent = flag' Silent ( long "silent"
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

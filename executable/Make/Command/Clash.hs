module Make.Command.Clash where

import Options.Applicative as Opt

import Make.Command.Common
import Make.HDL

data ClashCommand = ClashOptions
  { hdl :: (Maybe HDL)
  , clashTarget :: BuildTarget
  } deriving (Show)

clashCommand :: (ClashCommand -> a) -> Mod CommandFields a
clashCommand constructor = Opt.command "clash"
                           (info (constructor <$> clashCommandP)
                            (progDesc "Compile Haskell to an HDL using Clash"))

clashCommandP :: Parser ClashCommand
clashCommandP = ClashOptions <$> maybeP hdlP <*> buildTargetP

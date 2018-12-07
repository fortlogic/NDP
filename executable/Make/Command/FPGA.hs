module Make.Command.FPGA where

import Options.Applicative as Opt

import Make.Command.Common

data FPGACommand = FPGAReset | FPGABuildOptions
  { fpgaProgram :: Maybe FPGAProgram
  , fpgaTarget :: BuildTarget
  }deriving (Show)

data FPGAProgram = FPGAProgramLoad
                 | FPGAProgramBurn
                 deriving (Show)

fpgaCommand :: (FPGACommand -> a) -> Mod CommandFields a
fpgaCommand constructor = Opt.command "fpga"
              (info (constructor <$> (fpgaResetP <|> fpgaBuildOptionsP))
                (progDesc "Generate an FPGA bitfile"))

fpgaResetP :: Parser FPGACommand
fpgaResetP = flag' FPGAReset ( long "reset"
                               <> short 'r'
                               <> help "Reset the FPGA board")

fpgaBuildOptionsP :: Parser FPGACommand
fpgaBuildOptionsP = FPGABuildOptions <$> maybeP fpgaProgramP <*> buildTargetP

fpgaProgramP :: Parser FPGAProgram
fpgaProgramP = loadP <|> burnP
  where loadP = flag' FPGAProgramLoad ( long "load"
                                        <> help "Load the bitfile to the FPGA board")
        burnP = flag' FPGAProgramBurn ( long "burn"
                                        <> help "Burn the bitfile to the FPGA boards flash storage")

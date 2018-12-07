module Make.Command.Xilinx where

import Options.Applicative

import Make.Command.Common
import Make.HDL

data XilinxCommand = XilinxOptions
  { hdl :: Maybe HDL
  , xilinxTarget :: BuildTarget
  } deriving (Show)

xilinxCommand :: (XilinxCommand -> a) -> Mod CommandFields a
xilinxCommand constructor = command "xilinx"
                (info (constructor <$> xilinxCommandP)
                  (progDesc "Invoke the Xilinx ISE toolchain"))

xilinxCommandP :: Parser XilinxCommand
xilinxCommandP = XilinxOptions <$> maybeP hdlP <*> buildTargetP

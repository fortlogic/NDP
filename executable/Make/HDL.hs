module Make.HDL ( HDL (..)
                , parseHDL
                , hdlFlag
                , hdlName
                , hdlExtension ) where

import Data.Char

data HDL = VHDL | Verilog deriving (Read, Show, Eq)

parseHDL :: String -> Maybe HDL
parseHDL = parseHDL' . map toLower
  where parseHDL' "vhdl" = Just VHDL
        parseHDL' "verilog" = Just Verilog
        parseHDL' _ = Nothing

hdlFlag :: HDL -> String
hdlFlag VHDL = "--vhdl"
hdlFlag Verilog = "--verilog"

hdlName :: HDL -> String
hdlName VHDL = "vhdl"
hdlName Verilog = "verilog"

hdlExtension :: HDL -> String
hdlExtension VHDL = "vhdl"
hdlExtension Verilog = "v"

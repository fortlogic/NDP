module Make.HDL ( HDL (..)
                , parseHDL
                , parseHDL'
                , hdlFlag
                , hdlName
                , hdlExtension ) where

import Data.Char

data HDL = VHDL | Verilog deriving (Read, Show, Eq)

parseHDL :: String -> Maybe HDL
parseHDL = parse . map toLower
  where parse "vhdl" = Just VHDL
        parse "verilog" = Just Verilog
        parse _ = Nothing

parseHDL' :: String -> Either String HDL
parseHDL' = parse . parseHDL
  where parse Nothing = Left "expected 'vhdl' or 'verilog'"
        parse (Just hdl) = Right hdl

hdlFlag :: HDL -> String
hdlFlag VHDL = "--vhdl"
hdlFlag Verilog = "--verilog"

hdlName :: HDL -> String
hdlName VHDL = "vhdl"
hdlName Verilog = "verilog"

hdlExtension :: HDL -> String
hdlExtension VHDL = "vhdl"
hdlExtension Verilog = "v"

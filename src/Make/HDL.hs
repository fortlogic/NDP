module Make.HDL ( HDL (..)
                , hdlFlag
                , hdlName
                , hdlExtension ) where

data HDL = VHDL | Verilog deriving (Read, Show, Eq)

hdlFlag :: HDL -> String
hdlFlag VHDL = "--vhdl"
hdlFlag Verilog = "--verilog"

hdlName :: HDL -> String
hdlName VHDL = "vhdl"
hdlName Verilog = "verilog"

hdlExtension :: HDL -> String
hdlExtension VHDL = "vhdl"
hdlExtension Verilog = "v"

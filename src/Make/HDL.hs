module Make.HDL ( HDL (..)
                , hdlFlag
                , hdlName ) where

data HDL = VHDL deriving (Read, Show, Eq)

hdlFlag :: HDL -> String
hdlFlag VHDL = "--vhdl"

hdlName :: HDL -> String
hdlName VHDL = "vhdl"

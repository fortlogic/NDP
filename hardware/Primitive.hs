module Primitive ( HDL (..)
                 , ndp_primitive ) where

import Clash.Annotations.Primitive
import Paths_NDP
import System.FilePath
import System.IO.Unsafe

ndp_primitive :: HDL -> String -> Primitive
ndp_primitive hdl name = Primitive hdl $ unsafePerformIO (getDataFileName path)
  where path = "hardware" </> "Primitive" </> hdlName hdl </> name <.> "json"
        hdlName VHDL = "vhdl"
        hdlName Verilog = "verilog"
        hdlName SystemVerilog = "system-verilog"

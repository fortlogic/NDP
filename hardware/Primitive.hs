module Primitive ( HDL (..)
                 , ndp_primitive ) where

import Clash.Annotations.Primitive
import Paths_NDP
import System.IO.Unsafe
import System.FilePath

ndp_primitive :: HDL -> String -> Primitive
ndp_primitive hdl name = Primitive hdl $ unsafePerformIO (getDataFileName path)
  where path = "hardware" </> "hdl" </> hdlName hdl </> name <.> "json"
        hdlName VHDL = "vhdl"
        hdlName Verilog = "verilog"
        hdlName SystemVerilog = "system-verilog"

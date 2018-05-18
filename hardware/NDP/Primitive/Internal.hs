{-# LANGUAGE NoImplicitPrelude #-}
module Primitive ( HDL (..)
                 , ndp_primitive ) where

import Clash.Annotations.Primitive
import Paths_NDP
import Prelude
import System.FilePath
import System.IO.Unsafe

ndp_primitive :: HDL -> Primitive
ndp_primitive hdl = Primitive hdl $ unsafePerformIO (getDataFileName path)
  where path = "hardwared" </> "Primitive" </> hdlName hdl
        hdlName VHDL = "vhdl"
        hdlName Verilog = "verilog"
        hdlName SystemVerilog = "system-verilog"

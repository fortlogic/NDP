{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Xilinx.Primitive.DCM_CLKGEN ( dcm_clkgen# ) where

import Clash.Prelude
import GHC.Stack

-- * DCM_CLKGEN Ports
-- ** Inputs
--
--    | Port      | Width | Type   | Comment                                          |
--    |-----------+-------+--------+--------------------------------------------------|
--    | CLKIN     |     1 | Clock  | Input clock                                      |
--    | FREEZEDCM |     1 | Bool   | Locks DCM, CLKIN can be disconnected             |
--    | RST       |     1 | Bool   | Asynchronous reset                               |
--
-- *** CLKIN
--
--     Although it is possible for this signal to come from anywhere on the FPGA
--     it is discouraged. Acceptable sources are:
--
--     + A global clock input pin on the FPGA (GCLK)
--     + A global clock buffer (BUFG)
--     + A clock output from another DCM in the same Clock Management Tile
--     + A clock output from a PLL in the same Clock Management Tile
--
-- ** Outputs
--
--    | Port     | Width | Type  | Comment                                                               |
--    |----------+-------+-------+-----------------------------------------------------------------------|
--    | CLKFX    |     1 | Clock | Clock output ; 50% duty cycle ; NO phase relationship with CLKIN      |
--    | CLKFXDV  |     1 | Clock | Clock output divided again ; NO phase relationship with CLKFX         |
--    | CLKFX180 |     1 | Clock | Inverted clock output (divided & multiplied)                          |
--    | LOCKED   |     1 | Bool  | Clock outputs are stable ; true->false transition requires DCM reset  |
--    | STATUS   |     2 | Bits  | STATUS[1]: CLKIN has stopped ; STATUS[2]: outputs clocks have stopped |
--
-- * DCM_CLKGEN Attributes
--
--   | Attribute      | Type       | Domain        | Default   | Comment                                      |
--   |----------------+------------+---------------+-----------+----------------------------------------------|
--   | CLKFX_DIVIDE   | Int        | [1, 256]      |         1 | Initial CLKFX D value                        |
--   | CLKFXDV_DIVIDE | Int        | {2,4,8,16,32} |         2 | CLKFXDV D value                              |
--   | CLKFX_MULTIPLY | Int        | [2, 256]      |         4 | Initial CLKFX M value                        |
--   | CLKIN_PERIOD   | Float (ns) | [2, 1000]     | Undefined | Input clock period in nanoseconds            |
--   | STARTUP_WAIT   | Bool       | {True,False}  |     False | Delay PROGDONE signal until LOCKED goes high |


-- + The Xilinx guides also document a DFS_BANDWIDTH and PROG_MD_BANDWIDTH
--   attribute whose purpose is obscure and according to one of the guides are
--   reserved/private. They will not be exposed to Clash.
--
-- + Since Clash does not support variable clocks, the port and attributes
--   related to reprogramming M and D will be be hidden.
--
-- + The spread spectrum mode attribute will be hidden though this may change in
--   the future.

-- ^ DCM_CLKGEN primitive.
dcm_clkgen# :: ( HasCallStack
               , (i*m) ~ (o*d)
               , o' ~ (o*d') )
            => SNat m
            -> SNat d
            -> SNat d'
            -> Clock ('Dom dom0 i) gated -- ^ CLKIN
            -> Reset ('Dom dom0 i) 'Asynchronous -- ^ RST
            -> Signal ('Dom dom1 o) Bool -- ^ FREEZEDCM (can be driven by LOCKED)
            -> ( Clock ('Dom dom1 o) gated --  CLKFX
               , Clock ('Dom dom2 o') gated --  CLKFXDV
               , Clock ('Dom dom3 o) gated --  CLKFX180
               , Signal ('Dom dom1 o) Bool --  LOCKED
               , Signal ('Dom dom1 o) (BitVector 2)) -- ^ STATUS
-- dcm_clkgen# _ _ _ clkIn rstIn freeze = undefined
dcm_clkgen# = undefined

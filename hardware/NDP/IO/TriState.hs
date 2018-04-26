module NDP.IO.TriState where

import Prelude()

import Clash.Prelude

-- Input coming from a bidirectional IO pin
data In = LowI | HighI deriving (Show, Eq, Ord)

-- Convert In to Bit.
in2bit :: In -> Bit
in2bit LowI = low
in2bit HighI = high

-- Convert Bit to In.
bit2in :: Bit -> In
bit2in 0 = LowI
bit2in 1 = HighI
bit2in _ = error "illegal bit"

-- Output going to a bidirectional IO pin
data Out = LowO | HighO | SilentO deriving (Show, Eq, Ord)

-- The Xilinx IOBUF primitive
type IOBUF = Signal (Bool, Bit) -> Signal Bit

-- A tri-state IO pin
type Tri = Signal Out -> Signal In

-- A tri-state IO pin that only returns input when it knows that it's the only
-- person on the bus talking. Note that an error is only signalled if another
-- party is driving the bus in a different direction than we are. We cannot
-- detect conflict if another person is driving the bus in the same direction as
-- we are.
type ErrorTri = Signal Out -> Signal (Maybe In)

iobufInput :: Out -> (Bool, Bit)
iobufInput LowO    = (True, low)
iobufInput HighO   = (True, high)
iobufInput SilentO = (False, undefined)

-- Turn an IOBUF into a Tristate signal.
mkTri :: IOBUF -> Tri
mkTri buf tIn = bit2in <$> buf (iobufInput <$> tIn)

errorTri :: Tri -> ErrorTri
errorTri tri out = mkErr <$> out <*> input
  where input = tri out
        mkErr LowO  HighI  = Nothing
        mkErr HighO LowI   = Nothing
        mkErr _     input' = Just input'

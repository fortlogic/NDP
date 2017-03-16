module NDP.ADB.Raw where

import CLaSH.Prelude

data TriOut = LowO | HighO | SilentO

data TriIn = LowI | HighI

type TriState = (TriIn, TriOut)

triConflict :: TriState -> Bool
triConflict (LowI, LowO)     = False
triConflict (LowI, HighO)    = True
triConflict (LowI, SilentO)  = False
triConflict (HighI, LowO)    = True
triConflict (HighI, HighO)   = False
triConflict (HighI, SilentO) = False

-- States that shouldn't be physically possible.
triBroken :: TriState -> Bool
triBroken (LowI, HighO) = True
triBroken _ = False

data ADBOut = AttentionO
            | SyncO
            | DataBitO Bool
            | StopO
            | WaitO

data ADBIn = StartI
           | DataBitI Bool
           | StopI
           | WaitI

data ADBRequest = Req {
  address :: Unsigned 4,
  command :: Unsigned 2,
  register :: Unsigned 2
    }

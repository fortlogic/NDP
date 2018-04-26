module Tests.TestBench where

import Clash.Prelude

topEntity :: Signal Bool -> Signal Bool
topEntity s = not <$> s

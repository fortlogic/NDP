module Tests.TestBench where

import CLaSH.Prelude

topEntity :: Signal Bool -> Signal Bool
topEntity s = not <$> s

{-# LANGUAGE Arrows #-}
{-# LANGUAGE MagicHash #-}
module Signal (BoolS',
               SignalA' (SA, sa),
               SignalA,
               delayA') where

import CLaSH.Prelude hiding ((.), id)
import CLaSH.Signal.Explicit
import CLaSH.Signal.Internal
import Control.Category
import Control.Arrow
import Data.Default
import Data.Maybe
import Prelude hiding ((.), id)

type BoolS' clk = Signal' clk Bool

newtype SignalA' clk i o = SA {
  sa :: Signal' clk i -> Signal' clk o
  }
type SignalA i o = SignalA' SystemClock i o

delayA' :: a -> SignalA' clk a a
delayA' d = SA (register' undefined d)

instance Category (SignalA' clk) where
  id = SA (\ x -> x)
  (SA sa1) . (SA sa2) = SA (\ x -> sa1 (sa2 x))

-- Arrow Laws:
-- arr (id :: a -> a)                :=: (id :: SignalA' clk a a)
-- arr (f >>> g)                     :=: (arr f) >>> (arr g)
-- first . arr                       :=: arr . first
-- first (f >>> g)                   :=: (first f) >>> (first g)
-- (first f) >>> (arr fst)           :=: (arr fst) >>> f
-- (first f) >>> (arr (id *** g))    :=: (arr (id *** g)) >>> (first f)
-- (first (first f)) >>> (arr assoc) :=: (arr assoc) >>> (first f)

-- arr id :=: id
---- arr id                          -- premise
---- SA $ (<$>) id                   -- expand definition of `arr`
---- SA ((<$>) id)                   -- expand definition of `($)`
---- SA (fmap id)                    -- expand definition of `(<$>)`
---- SA (id :: Signal a -> Signal a) -- apply (fmap :: (a -> b) -> Signal a -> Signal b)
----                                    to (id :: a -> a)
---- id :: SignalA' clk a a          -- apply (SA :: (Signal a -> Signal b) -> SignalA a b)
---- arr id :=: id                   -- conclusion

-- arr (f >>> g) :=: (arr f) >>> (arr g)
---- arr (f >>> g)                         -- premise
---- SA $ (<$>) (f >>> g)                  -- expand definition of `arr` over `SignalA'`
---- SA $ fmap (f >>> g)                   -- expand definition of `(<$>)`
---- SA (fmap (f >>> g))                   -- expand definition of `($)`
---- SA (fmap (g . f))                     -- expand definition of `(>>>)`
---- SA (fmap g . fmap f)                  -- Data.Functor law 2
---- SA (\ x -> (fmap g) ((fmap f) x))     -- expand definition of `(.)` over `(->)`
---- (SA (fmap g)) . (SA (fmap f))         -- contract definition of `(.)` over `SignalA'`
---- (SA (fmap f)) >>> (SA (fmap g))       -- contract definition of `(>>>)`
---- (SA ((<$>) f)) >>> (SA ((<$>) g))     -- contract definition of `(<$>)`
---- (SA $ (<$>) f) >>> (SA $ (<$>) g)     -- contract definition of `($)`
---- (arr f) >>> (arr g)                   -- contract definition of `arr`
---- arr (f >>> g) :=: (arr f) >>> (arr g) -- conclusion

-- TODO: Still to be proven:
-- first . arr                       :=: arr . first
-- first (f >>> g)                   :=: (first f) >>> (first g)
-- (first f) >>> (arr fst)           :=: (arr fst) >>> f
-- (first f) >>> (arr (id *** g))    :=: (arr (id *** g)) >>> (first f)
-- (first (first f)) >>> (arr assoc) :=: (arr assoc) >>> (first f)

instance Arrow (SignalA' clk) where
  arr f = SA $ (<$>) f
  first (SA f) = SA (\ inS -> let ~(l, r) = unbundle' undefined inS
                              in bundle' undefined (f l, r))

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

fromLeft :: (Either a b) -> Maybe a
fromLeft (Left a) = Just a
fromLeft _ = Nothing

fromRight :: (Either a b) -> Maybe b
fromRight (Right b) = Just b
fromRight _ = Nothing

-- TODO: Still to be proven
-- ArrowChoice Laws:
-- left (arr f) :=: arr (left f)
-- left (f >>> g)                 :=: left f >>> left g
-- f >>> arr Left                 :=: arr Left >>> left f
-- left f >>> arr (id +++ g)      :=: arr (id +++ g) >>> left f
-- left (left f) >>> arr assocsum :=: arr assocsum >>> left f

instance ArrowChoice (SignalA' clk) where
  left (SA f) = SA (\ inS ->
                       let l = Left <$> f (fromJust <$> (fromLeft <$> inS))
                           r = Right <$> (fromJust <$> (fromRight <$> inS))
                           res = mux (isLeft <$> inS) l r
                       in res)

-- ArrowLoop Laws:
-- loop (arr f)                :=: arr (\ b -> fst (fix (\ (c,d) -> f (b,d))))
-- loop (first h >>> f)        :=: h >>> loop f
-- loop (f >>> first h)        :=: loop f >>> h
-- loop (f >>> arr (id *** k)) :=: loop (arr (id *** k) >>> f)
-- loop (loop f)               :=: loop (arr unassoc >>> f >>> arr assoc)
-- second (loop f)             :=: loop (arr assoc >>> second f >>> arr unassoc)

instance ArrowLoop (SignalA' clk) where
  loop (SA f) = SA (\ b -> let ~(c, d) = unbundle' undefined (f (bundle' undefined (b, d)))
                           in c)


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module NDP.Utils.Type where

import GHC.TypeLits
import Prelude

type Mult a b = a GHC.TypeLits.* b

type family IfThenElse (c :: Bool) (t :: a) (f :: a) :: a where
  IfThenElse 'True  x _ = x
  IfThenElse 'False _ x = x

type Divides n m = (Mod n m) ~ 0

type family DivisibleError (n :: Nat) (m :: Nat) :: a where
  DivisibleError n m = TypeError ('ShowType n ':<>: 'Text " is not divisible by " ':<>: 'ShowType m ':<>: 'Text ".")

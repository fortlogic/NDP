{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module NDP.Utils.Type where

import Data.Type.Equality as TEQ
import GHC.TypeLits
import Prelude

type family IfThenElse (c :: Bool) (t :: a) (f :: a) :: a where
  IfThenElse 'True  x _ = x
  IfThenElse 'False _ x = x

type family Divides (n :: Nat) (m :: Nat) :: Bool where
  Divides n m = (Mod n m) TEQ.== 0

type family DivisibleError (n :: Nat) (m :: Nat) :: a where
  DivisibleError n m = TypeError ('ShowType n ':<>: 'Text " is not divisible by " ':<>: 'ShowType m ':<>: 'Text ".")

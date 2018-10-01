{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module NDP.Clocking.Domains where

import Clash.Prelude
--import Data.Singletons.TH

import NDP.Utils.Type

-- Clock rates signals period in picoseconds

--
-- NDP Clock Domains
--

-- External clock source is 50MHz
type OutsideD = 'Dom "Raw" 20000

-- Pixel clock is 40MHz
type PixelD = 'Dom "HDMI" 25000
-- TMDS clock is 200MHz (5 * pixel clock)
type Pixel5xD  = SquishDomain PixelD 5

--
-- Clock Domain Type Families
--

-- The name of a particular domain
type family DomainName (d :: Domain) :: Symbol where
  DomainName ('Dom n _) = n

-- the period of a particular domain
type family DomainPeriod (d :: Domain) :: Nat where
  DomainPeriod ('Dom _ p) = p

-- a family that stretches a clock, slowing it down by factor.
type family StretchDomain (d :: Domain) (n :: Nat) :: Domain where
  StretchDomain ('Dom n p) s = 'Dom n (p*s)

-- a family that squishes a clock, as long as the period is divisible by the speedup factor.
type family SquishDomain (d :: Domain) (n :: Nat) :: Domain where
  SquishDomain ('Dom n p) s = IfThenElse (Divides p s) ('Dom n (Div p s)) (DivisibleError p s)

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
module NDP.Clocking.Domains.Builtin ( Builtin
                                    , vBuiltin
                                    ) where

import Clash.Prelude



-- External clock source is 50MHz

createDomain VDomainConfiguration
  { vName          = "Builtin"
  , vPeriod        = 20000 -- 50MHz
  , vActiveEdge    = Rising
  , vResetKind     = Synchronous
  , vInitBehavior  = Defined
  , vResetPolarity = ActiveHigh
  }


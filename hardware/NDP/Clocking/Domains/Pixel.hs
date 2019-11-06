{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module NDP.Clocking.Domains.Pixel ( Pixel
                                  , vPixel ) where

import Clash.Prelude

import NDP.Clocking.Domains.Builtin

-- Pixel clock is 40MHz
createDomain vBuiltin
  { vName          = "Pixel"
  , vPeriod        = 25000 -- 40MHz
  }

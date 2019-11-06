{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module NDP.Clocking.Domains.TMDS ( TMDS
                                 , vTMDS ) where

import Clash.Prelude

import NDP.Clocking.Domains.Pixel

-- TMDS clock is 200MHz (5 * pixel clock)
createDomain vPixel
  { vName          = "TMDS"
  , vPeriod        = 5000 -- 200MHz
  }

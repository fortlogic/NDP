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
module NDP.Clocking.Domains ( Builtin
                            , Pixel
                            , TMDS
                            , vBuiltin
                            , vPixel
                            , vTMDS
                            ) where

-- import NDP.Utils.Type
import NDP.Clocking.Domains.Builtin
import NDP.Clocking.Domains.Pixel
import NDP.Clocking.Domains.TMDS

-- Clock rates signals period in picoseconds

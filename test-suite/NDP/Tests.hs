{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module NDP.Tests where

import Test.Hspec

import NDP.Tests.Clash

ndpTests :: Spec
ndpTests = do
  clashTests "Clash"



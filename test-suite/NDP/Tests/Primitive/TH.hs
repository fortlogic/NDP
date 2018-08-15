{-# LANGUAGE TemplateHaskell #-}
module NDP.Tests.Primitive.TH where

import Data.Maybe
import Language.Haskell.TH

qualified :: Name -> Lit
qualified n = StringL (moduleName ++ "." ++ nameBase n)
  where moduleName = fromMaybe "" (nameModule n)

mkTestName :: Name -> (String -> Name)
mkTestName orig n = mkName (mconcat ["NDP.Tests.Primitive.", modName, ".", n])
  where modName = (reverse . takeWhile (/= '.') . reverse . fromJust . nameModule) orig

testPrimitiveNamed :: Name -> Q Exp
testPrimitiveNamed n = let mkName' = mkTestName n in
  [| describe $(litE (qualified n)) $ do
      $(varE (mkName' "properties")) "QuickCheck Invariants"
      describe "Clash Testbench" $ do
        it "should do stuff" $ pendingWith "TODO: finish template haskell"
      describe "VHDL Generation" $ do
        it "should do stuff" $ pendingWith "TODO: finish template haskell"
      describe "VHDL Testbench" $ do
        it "should do stuff" $ pendingWith "TODO: finish template haskell" |]

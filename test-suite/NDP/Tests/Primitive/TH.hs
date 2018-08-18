{-# LANGUAGE TemplateHaskell #-}
module NDP.Tests.Primitive.TH ( mkPrimitiveTestSuite
                              , mkPrimitiveTest ) where

import Data.Maybe
import Language.Haskell.TH

qualified :: Name -> String
qualified n = moduleName ++ "." ++ nameBase n
  where moduleName = fromMaybe "" (nameModule n)

qualifiedLit :: Name -> Q Exp
qualifiedLit = litE . stringL . qualified

boolE :: Bool -> Q Exp
boolE True = [| True |]
boolE False = [| False |]

mkQualifiedName :: Name -> String -> Q (Maybe Name)
mkQualifiedName orig n = lookupValueName (mconcat ["NDP.Tests.Primitive.", modName, ".", n])
  where modName = (reverse . takeWhile (/= '.') . reverse . fromJust . nameModule) orig

mkPrimitiveTestSuite :: String -> [Name] -> Q Exp
mkPrimitiveTestSuite suiteName primitiveNames =
  [| describe $(stringE suiteName) $(doE (map (noBindS . mkPrimitiveTest) primitiveNames)) |]

mkPrimitiveTest :: Name -> Q Exp
mkPrimitiveTest n = do
  nameLit <- qualifiedLit n
  maybeQCName <- mkQualifiedName n "properties"
  maybeTBName <- mkQualifiedName n "testbench"
  [|
    describe $(pure nameLit) $ do
      it "should have QuickCheck suite" $(boolE (isJust maybeQCName))
      ($(case maybeQCName of
           Nothing -> [| return () |]
           Just qcName -> [| $(varE qcName) "QuickCheck invariants" |] ))
      it "should have a testbench?" $(boolE (isJust maybeTBName))
      it "should run the testbench with clash?" $ do
        pendingWith "in an ideal world maybe..."
      it "should be able to generate a VHDL testbench?" $ do
        pendingWith "in an ideal world maybe..."
      it "should run the testbench with ghdl?" $ do
        pendingWith "in an ideal world maybe..."
     {- it "should be able to generate a Verilog testbench?" $ do
        pendingWith "in an ideal world maybe..."
      it "should have run the testbench with icarus?" $ do
        pendingWith "in an ideal world maybe..."-} |]


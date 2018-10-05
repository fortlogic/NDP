module Test.Tasty.Pending ( pending
                          , pending'
                          , Pending (..) ) where

import Data.Proxy
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Providers
import qualified Test.Tasty.Hspec as TastySpec

pending :: TestName -> String -> TestTree
pending name msg = singleTest name (Pending (Just msg))

pending' :: TestName -> TestTree
pending' name = singleTest name (Pending Nothing)


data Pending = Pending (Maybe String)
             deriving (Read,Show,Eq,Ord)

instance IsTest Pending where
  run opts (Pending msg') _ = let
    msg = constructMessage msg' in
      case lookupOption opts of
        TastySpec.Success -> return $ testPassed msg
        TastySpec.Failure -> return $ testFailed msg

  testOptions = return
    [ Option (Proxy :: Proxy TastySpec.TreatPendingAs) ]

constructMessage :: Maybe String -> String
constructMessage Nothing = "Pending"
constructMessage (Just msg) = "Pending: " ++ msg

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Make.Vagrant (withVagrant,
                     VagrantStatus (..),
                     vagrantStatusAddOracle,
                     vagrantStatusIO) where

import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.Classes

newtype VagrantStatus = VagrantStatus ()
                      deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

vagrantStatusAddOracle :: Rules (VagrantStatus -> Action (Maybe String))
vagrantStatusAddOracle = addOracle $ \ (VagrantStatus _) -> do
  status <- liftIO vagrantStatusIO
  if statusRunning status
    then return status
    else startupVagrant >> liftIO vagrantStatusIO

vagrantStatusIO :: IO (Maybe String)
vagrantStatusIO = do
  rawStatus <- (fromStdout <$> cmd "vagrant status")
  return $ do -- entering the Maybe monad
    line <- find (isPrefixOf "default    ") $ lines rawStatus
    (return . dropWhile (==' ') . drop 7) line

withVagrant :: Action a -> Action a
withVagrant action = do
  vagrantStatus <- askOracleWith (VagrantStatus ()) (Just "")
  action

statusRunning :: Maybe String -> Bool
statusRunning = fromMaybe False . (isPrefixOf "running " <$>)


startupVagrant :: Action ()
startupVagrant = cmd "vagrant up"


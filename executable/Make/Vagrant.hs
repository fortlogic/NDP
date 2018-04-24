{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Make.Vagrant (withVagrant,
                     VagrantStatus (..),
                     vagrantStatusAddOracle,
                     vagrantStatusIO,
                     vagrantSSH) where

import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.Classes
import System.Posix.Escape

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
withVagrant act = do
  vagrantStatus <- askOracleWith (VagrantStatus ()) (Just "")
  if statusRunning vagrantStatus
    then act
    else error "Unable to start Vagrant VM"

statusRunning :: Maybe String -> Bool
statusRunning = fromMaybe False . (isPrefixOf "running " <$>)


startupVagrant :: Action ()
startupVagrant = cmd "vagrant up"

vagrantSSH :: [String] -> Action ()
vagrantSSH args = cmd Shell "vagrant ssh -c" [escape $ intercalate " " args]

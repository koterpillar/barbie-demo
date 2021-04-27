{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens

import Control.Monad (when)

import Data.Monoid (Last(..))

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Lens (unpacked)

import Generic.Data (Generic, Generically(..))

import System.Environment (getArgs)
import System.Exit (die)
import System.IO (IOMode(..), hGetContents, withFile)

data LogLevel
  = Quiet
  | Normal
  | Verbose
  deriving (Ord, Eq)

data Config =
  Config
    { _logLevel :: LogLevel
    , _file :: Text
    }

makeLenses ''Config

data PartialConfig =
  PartialConfig
    { _plogLevel :: Last LogLevel
    , _pfile :: Last Text
    }
  deriving (Generic)
  deriving Semigroup via (Generically PartialConfig)
  deriving Monoid via (Generically PartialConfig)

makeLenses ''PartialConfig

mkLast :: a -> Last a
mkLast = Last . Just

defaultConfig :: PartialConfig
defaultConfig = mempty & plogLevel .~ mkLast Normal

commandLineConfig :: IO PartialConfig
commandLineConfig = go mempty <$> getArgs
  where
    go c [] = c
    go c ("--quiet":args) = go (c & plogLevel .~ mkLast Quiet) args
    go c ("--verbose":args) = go (c & plogLevel .~ mkLast Verbose) args
    go c ("--file":file:args) = go (c & pfile .~ mkLast (Text.pack file)) args

fromPartialConfig :: PartialConfig -> Maybe Config
fromPartialConfig PartialConfig {..} =
  getLast $ do
    _logLevel <- _plogLevel
    _file <- _pfile
    pure Config {..}

main :: IO ()
main = do
  let config1 = defaultConfig
  config2 <- commandLineConfig
  let config' = config1 <> config2
  case fromPartialConfig config' of
    Just config -> act config
    Nothing -> die "No configuration"

log_ :: Config -> LogLevel -> Text -> IO ()
log_ config level message =
  when (config ^. logLevel >= level) $ Text.putStrLn message

logDebug, logInfo :: Config -> Text -> IO ()
logDebug config = log_ config Verbose

logInfo config = log_ config Normal

act :: Config -> IO ()
act config = do
  logDebug config "Starting"
  withFile (config ^. file . unpacked) ReadMode $ \handle -> do
    logInfo config $ "Catting file: " <> config ^. file
    text <- Text.hGetContents handle
    Text.putStr text
  logDebug config "Finishing"

{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Lens

import           Control.Monad  (when)

import           Data.Text      (Text)
import qualified Data.Text.IO   as Text
import           Data.Text.Lens (unpacked)

import           System.Exit    (die)
import           System.IO      (IOMode (..), hGetContents, withFile)

data LogLevel
  = Quiet
  | Normal
  | Verbose
  deriving (Ord, Eq)

data Config =
  Config
    { _logLevel :: LogLevel
    , _file     :: Text
    }

makeLenses ''Config

data PartialConfig =
  PartialConfig
    { _plogLevel :: Maybe LogLevel
    , _pfile     :: Maybe Text
    }

fromPartialConfig :: PartialConfig -> Maybe Config
fromPartialConfig PartialConfig {..} = do
  _logLevel <- _plogLevel
  _file <- _pfile
  pure Config {..}

defaultConfig :: PartialConfig
defaultConfig = PartialConfig {_plogLevel = Just Normal, _pfile = Nothing}

main :: IO ()
main = do
  let config' = defaultConfig
  case fromPartialConfig config' of
    Just config -> act config
    Nothing     -> die "No configuration"

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

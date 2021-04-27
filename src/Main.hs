{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Lens

import           Data.Text      (Text)
import qualified Data.Text.IO   as Text
import           Data.Text.Lens (unpacked)

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

defaultConfig :: Config
defaultConfig = Config {_logLevel = Normal, _file = "test.txt"}

main :: IO ()
main = act defaultConfig

act :: Config -> IO ()
act config =
  withFile (config ^. file . unpacked) ReadMode $ \handle -> do
    text <- Text.hGetContents handle
    Text.putStr text

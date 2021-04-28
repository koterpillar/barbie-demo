{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Barbies

import           Control.Lens

import           Control.Monad      (when)

import           Data.Map           (Map)
import qualified Data.Map           as Map

import           Data.Monoid        (Last (..))

import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text
import           Data.Text.Lens     (unpacked)

import           Generic.Data       (Generic, Generically (..), gcompare)

import           System.Environment (getArgs, lookupEnv)
import           System.Exit        (die)
import           System.IO          (IOMode (..), hGetContents, withFile)

(?>~) :: ASetter s t a (Last b) -> b -> s -> t
l ?>~ t = set l (Last $ Just t)

identity :: Lens (Identity a) (Identity b) a b
identity f (Identity a) = Identity <$> f a

data FromEnvironment a where
  FromEnvironmentText :: String -> FromEnvironment Text
  FromEnvironmentMap :: String -> Map String a -> FromEnvironment a

fromEnvironment :: TraversableB b => b FromEnvironment -> IO (b Last)
fromEnvironment = btraverse go
  where
    go :: FromEnvironment a -> IO (Last a)
    go (FromEnvironmentText var) = fmap Text.pack <$> lastEnv var
    go (FromEnvironmentMap var values) = (>>= (\a -> Last $ Map.lookup a values)) <$> lastEnv var
    lastEnv :: String -> IO (Last String)
    lastEnv var = Last <$> lookupEnv var

fromPartial :: TraversableB b => b Last -> Maybe (b Identity)
fromPartial = getLast . bsequence'

data LogLevel
  = Quiet
  | Normal
  | Verbose
  deriving (Ord, Eq, Show)

data Config' f =
  Config
    { _logLevel :: f LogLevel
    , _file     :: f Text
    }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

deriving instance AllBF Show f Config' => Show (Config' f)

deriving instance AllBF Eq f Config' => Eq (Config' f)

deriving via (Barbie Config' f) instance
         AllBF Semigroup f Config' => Semigroup (Config' f)

deriving via (Barbie Config' f) instance
         AllBF Monoid f Config' => Monoid (Config' f)

makeLenses ''Config'

type Config = Config' Identity

type PartialConfig = Config' Last

defaultConfig :: PartialConfig
defaultConfig = mempty & logLevel ?>~ Normal

commandLineConfig :: IO PartialConfig
commandLineConfig = go mempty <$> getArgs
  where
    go c []                  = c
    go c ("--quiet":args)    = go (c & logLevel ?>~ Quiet) args
    go c ("--verbose":args)  = go (c & logLevel ?>~ Verbose) args
    go c ("--file":arg:args) = go (c & file ?>~ Text.pack arg) args

environmentConfig :: Config' FromEnvironment
environmentConfig =
  Config
    { _logLevel =
        FromEnvironmentMap "LOG_LEVEL" $
        Map.fromList [("quiet", Quiet), ("verbose", Verbose)]
    , _file = FromEnvironmentText "FILE"
    }

main :: IO ()
main = do
  let config1 = defaultConfig
  config2 <- fromEnvironment environmentConfig
  config3 <- commandLineConfig
  let config' = config1 <> config2 <> config3
  case fromPartial config' of
    Just config -> act config
    Nothing     -> die "No configuration"

log_ :: Config -> LogLevel -> Text -> IO ()
log_ config level message =
  when (config ^. logLevel . identity >= level) $ Text.putStrLn message

logDebug, logInfo :: Config -> Text -> IO ()
logDebug config = log_ config Verbose

logInfo config = log_ config Normal

act :: Config -> IO ()
act config = do
  logDebug config "Starting"
  withFile (config ^. file . identity . unpacked) ReadMode $ \handle -> do
    logInfo config $ "Catting file: " <> config ^. file . identity
    text <- Text.hGetContents handle
    Text.putStr text
  logDebug config "Finishing"

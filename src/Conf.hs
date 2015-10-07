{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Conf (Conf(..), Config(..), friendlyConf) where

import GHC.Generics
import System.Posix.Types
import Data.Aeson hiding (Value)
import Data.Aeson.Types hiding (Value)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Haskakafka.InternalSetup


type Topic = String
type Setting = String
type Value = String
type Username = String
type Password = String


type KafkaConf = Map Setting Value

type KafkaTopicsConf = Map Topic KafkaConf

type AuthConf = Map Username Password


data Conf = Conf {
  kafka :: KafkaConf,
  kafkaTopicDefaults :: KafkaConf,
  kafkaTopics :: KafkaTopicsConf,
  basicAuth :: AuthConf
} deriving (Eq, Ord, Show, Generic)

instance ToJSON Conf
instance FromJSON Conf



data Config = Config {
  kafkaConfig :: ConfigOverrides,
  kafkaTopicDefaultsConfig :: ConfigOverrides,
  kafkaTopicsConfig :: Map String ConfigOverrides,
  basicAuthConfig :: Map ByteString ByteString
} deriving (Eq, Ord, Show)


friendlyConf c = Config {
    kafkaConfig = kafkaConf,
    kafkaTopicDefaultsConfig = kafkaTopicDefaultsConf,
    kafkaTopicsConfig = kafkaTopicsConf,
    basicAuthConfig = basicAuthConf
  }
  where
    kafkaConf = Map.assocs (kafka c) :: ConfigOverrides
    kafkaTopicDefaultsConf = Map.assocs (kafkaTopicDefaults c) :: ConfigOverrides
    kafkaTopicsConf = Map.map Map.assocs (kafkaTopics c)
    basicAuthConf = Map.map BS.pack $ Map.mapKeys BS.pack $ basicAuth c

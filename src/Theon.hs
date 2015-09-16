{-# LANGUAGE OverloadedStrings #-}
 
module Theon (Options(..), Mode(..), versionMode, normalMode) where

import Paths_theon (version)
import Data.Version (showVersion)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Haskakafka
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad (replicateM_)


data Options = Options {
  broker :: String,
  port :: Int
} deriving (Eq, Ord, Show)


data Mode = VersionMode | NormalMode Options deriving (Eq, Ord, Show)

type TopicEvent = (ByteString, ByteString)

type TopicMap = Map ByteString KafkaTopic

contentTypePlain = [ ("Content-Type", "text/plain") ]


versionMode :: IO ()
versionMode = putStrLn $ showVersion version


normalMode :: Options -> IO ()
normalMode opts = do
  chan <- newChan
  withKafkaProducer [] [] (broker opts) ".theon" $
    \kafka _ -> forkIO $ process chan kafka Map.empty
  run (port opts) (app chan)


process :: Chan TopicEvent -> Kafka -> TopicMap -> IO ()
process chan kafka topics = do
  (topicName, event) <- readChan chan
  let message = KafkaProduceMessage event
  case Map.lookup topicName topics of
    Nothing -> do
      topic <- newKafkaTopic kafka (BS.unpack topicName) []
      produceMessage topic KafkaUnassignedPartition message
      let updatedTopics = Map.insert topicName topic topics
      process chan kafka updatedTopics
    Just topic -> do
      produceMessage topic KafkaUnassignedPartition message
      process chan kafka topics


app :: Chan TopicEvent -> Application
app chan req respond = do
  rawEvents <- requestBody req
  let topic = BS.drop 1 $ rawPathInfo req
  let events = BS.split '\n' rawEvents
  let topicEvents = map (\e -> (topic, e)) events
  mapM_ (writeChan chan) topicEvents
  respond $
    responseLBS status200 contentTypePlain ""

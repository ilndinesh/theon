{-# LANGUAGE OverloadedStrings #-}
 
module Theon (Options(..), Mode(..), versionMode, normalMode) where

import Paths_theon (version)
import Data.Version (showVersion)

import Haskakafka
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status401)
import Network.Wai.Middleware.HttpAuth
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (lookup)


data Options = Options {
  broker :: String,
  port :: Int,
  auth :: Bool,
  user :: String,
  pass :: String
} deriving (Eq, Ord, Show)


data Mode = VersionMode | NormalMode Options deriving (Eq, Ord, Show)

type Topic = ByteString

type Message = ByteString

type ProduceEvent = (Topic, [Message])

type TopicMap = Map Topic KafkaTopic

contentTypePlain = [ ("Content-Type", "text/plain") ]


versionMode :: IO ()
versionMode = putStrLn $ showVersion version


normalMode :: Options -> IO ()
normalMode opts = do
  putStrLn $ show opts
  chan <- newChan
  withKafkaProducer [] [] (broker opts) ".theon" $
    \kafka _ -> forkIO $ process chan kafka Map.empty
  case auth opts of
    False -> run (port opts) (app chan)
    True -> do
      let u = BS.pack $ user opts
      let p = BS.pack $ pass opts
      run (port opts) $ checkAuthorization u p (app chan)


process :: Chan ProduceEvent -> Kafka -> TopicMap -> IO ()
process chan kafka topics = do
  (topicName, rawMessages) <- readChan chan
  let messages = map (\m -> KafkaProduceMessage m) rawMessages
  case Map.lookup topicName topics of
    Nothing -> do
      topic <- newKafkaTopic kafka (BS.unpack topicName) []
      produceMessageBatch topic KafkaUnassignedPartition messages
      let updatedTopics = Map.insert topicName topic topics
      process chan kafka updatedTopics
    Just topic -> do
      produceMessageBatch topic KafkaUnassignedPartition messages
      process chan kafka topics


checkAuthorization :: ByteString -> ByteString -> Middleware
checkAuthorization user pass =
  basicAuth (\u p -> return $ u == user && p == pass) "theon"


app :: Chan ProduceEvent -> Application
app chan req respond = do
  rawMessages <- requestBody req
  let messages = BS.split '\n' rawMessages
  let topic    = BS.drop 1 $ rawPathInfo req
  writeChan chan (topic, messages)
  respond $
    responseLBS status200 contentTypePlain ""

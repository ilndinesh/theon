{-# LANGUAGE OverloadedStrings #-}
 
module Theon (Options(..), Mode(..), versionMode, normalMode) where

import Paths_theon (version)
import Data.Version (showVersion)

import Haskakafka
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan


data Options = Options {
  broker :: String,
  topic :: String,
  delay :: Int,
  port :: Int
} deriving (Eq, Ord, Show)


data Mode = VersionMode | NormalMode Options deriving (Eq, Ord, Show)

contentTypePlain = [ ("Content-Type", "text/plain") ]


versionMode :: IO ()
versionMode = putStrLn $ showVersion version


normalMode :: Options -> IO ()
normalMode opts = do
  chan <- newChan
  withKafkaProducer [] [] (broker opts) (topic opts) $
    \kafka topic -> forkIO $ process chan topic
  run (port opts) (app chan)


process :: Chan ByteString -> KafkaTopic -> IO ()
process chan topic = do
  event <- readChan chan
  let message = KafkaProduceMessage event
  produceMessage topic KafkaUnassignedPartition message
  process chan topic


app :: Chan ByteString -> Application
app chan req respond = do
  event <- requestBody req
  writeChan chan event
  respond $
    responseLBS status200 contentTypePlain ""

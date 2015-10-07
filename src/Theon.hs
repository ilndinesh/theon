{-# LANGUAGE OverloadedStrings #-}
 
module Theon (Options(..), Mode(..), versionMode, normalMode) where

import Paths_theon (version)
import Data.Version (showVersion)

import Conf

import Haskakafka
import Haskakafka.InternalSetup
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status401)
import qualified Network.Wai.Middleware.HttpAuth as Auth
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Char8 (ByteString)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List
import Data.List (lookup)
import System.Posix.Types
import Data.Aeson as JSON (encode, decode)
import Text.Regex
import Text.Regex.Posix.ByteString.Lazy


data Options = Options {
  broker :: String,
  port :: Int,
  config :: FilePath
} deriving (Eq, Ord, Show)


data Mode = VersionMode | NormalMode Options deriving (Eq, Ord, Show)

type Topic = ByteString

type Message = ByteString

type ProduceEvent = (Topic, [Message])

type TopicMap = Map Topic KafkaTopic

contentTypePlain = [ ("Content-Type", "text/plain") ]


versionMode :: IO ()
versionMode = putStrLn $ showVersion version


jsonConf :: BL.ByteString -> Maybe Conf
jsonConf rawConf = JSON.decode (BL.pack rawConfNocomment)
  where
    jsonCommentLines = mkRegex "[:blank:]*//.*"
    rawConfNocomment = subRegex jsonCommentLines (BL.unpack rawConf) ""


normalMode :: Options -> IO ()
normalMode opts = do
  rawConf <- BL.readFile $ config opts
  case jsonConf rawConf of
    Nothing -> putStrLn "Sorry, your config didn't parse!"
    Just conf -> main opts $ friendlyConf conf


kafkaTopicConfig :: ConfigOverrides -> Map String ConfigOverrides -> String -> ConfigOverrides
kafkaTopicConfig defaults topicsConf topic =
  List.union defaults topicConf
  where
    topicConf = Map.findWithDefault defaults topic topicsConf


main :: Options -> Config -> IO ()
main opts conf = do
  putStrLn $ show opts
  putStrLn $ show conf

  let kafkaConf = kafkaConfig conf
  let kafkaTopicDefaultsConf = kafkaTopicDefaultsConfig conf
  let kafkaTopicsConf = kafkaTopicsConfig conf
  let basicAuthConf = basicAuthConfig conf
  let performBasicAuth = not $ Map.null basicAuthConf

  chan <- newChan

  withKafkaProducer kafkaConf [] (broker opts) ".theon" $
    \kafka _ -> forkIO $
      process chan kafka Map.empty kafkaTopicDefaultsConf kafkaTopicsConf

  case performBasicAuth of
    False -> run (port opts) (app chan)
    True -> run (port opts) $
      checkAuthorization basicAuthConf $ app chan


process :: Chan ProduceEvent -> Kafka -> TopicMap -> ConfigOverrides -> Map String ConfigOverrides -> IO ()
process chan kafka topics kafkaTopicDefaultsConf kafkaTopicsConf = do
  (topicName, rawMessages) <- readChan chan
  let messages = map (\m -> KafkaProduceMessage m) rawMessages
  case Map.lookup topicName topics of
    Nothing -> do
      let topicString = BS.unpack topicName
      let topicConfig = kafkaTopicConfig kafkaTopicDefaultsConf kafkaTopicsConf topicString
      topic <- newKafkaTopic kafka topicString topicConfig
      produceMessageBatch topic KafkaUnassignedPartition messages
      let updatedTopics = Map.insert topicName topic topics
      process chan kafka updatedTopics kafkaTopicDefaultsConf kafkaTopicsConf
    Just topic -> do
      produceMessageBatch topic KafkaUnassignedPartition messages
      process chan kafka topics kafkaTopicDefaultsConf kafkaTopicsConf


checkAuthorization :: Map ByteString ByteString -> Middleware
checkAuthorization userPasses =
  Auth.basicAuth (\u p -> return $ userPass u == p) "theon"
  where
    userPass u = Map.findWithDefault "" u userPasses


app :: Chan ProduceEvent -> Application
app chan req respond = do
  rawMessages <- requestBody req
  let messages = BS.split '\n' rawMessages
  let topic    = BS.drop 1 $ rawPathInfo req
  writeChan chan (topic, messages)
  respond $
    responseLBS status200 contentTypePlain ""

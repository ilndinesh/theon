{-# LANGUAGE OverloadedStrings #-}
 
module Theon (Options(..), Mode(..), versionMode, normalMode) where

import Paths_theon (version)
import Data.Version (showVersion)

import Haskakafka
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import qualified Database.LevelDB as LDB
import Database.LevelDB (DB)
import Database.LevelDB.Iterator
import Control.Monad (when)
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)
import Control.Exception.Base
import Data.Default (def)
import Data.Binary as B
import Data.Bits (shift)


data Options = Options {
  journal :: FilePath,
  broker :: String,
  topic :: String,
  delay :: Int,
  port :: Int
} deriving (Eq, Ord, Show)


data Mode = VersionMode | NormalMode Options deriving (Eq, Ord, Show)

type Cursor = MVar Int

intFrom :: Maybe BS.ByteString -> Int
intFrom Nothing  = 0
intFrom (Just a) = B.decode $ BL.fromStrict a

contentTypePlain = [ ("Content-Type", "text/plain") ]
success = ""


versionMode :: IO ()
versionMode = putStrLn $ showVersion version



normalMode :: Options -> IO ()
normalMode opts = runResourceT $ do
  db <- LDB.open (journal opts) def {
    LDB.createIfMissing = True,
    LDB.cacheSize       = 4 `shift` 20, -- 4 MB
    LDB.writeBufferSize = 8 `shift` 20  -- 8 MB
  }

  rawSeq <- LDB.get db def "seq"
  seqRef <- liftIO $ newMVar (intFrom rawSeq)
  let seq = intFrom rawSeq

  rawCur <- LDB.get db def "cur"
  curRef <- liftIO $ newMVar (intFrom rawCur)
  let cur = intFrom rawCur

  liftIO $ forkIO $ reportStats curRef seqRef
  liftIO $ withKafkaProducer [] [] (broker opts) (topic opts) $
    \kafka topic -> forkIO $ process db curRef (delay opts) kafka topic
  liftIO $ putStrLn $ (show opts) ++ ", Seq " ++ (show seq) ++ ", Cur " ++ (show cur)
  liftIO $ run (port opts) (app db seqRef)



reportStats :: Cursor -> Cursor -> IO ()
reportStats curRef seqRef = do
  threadDelay 5000000
  cur <- readMVar curRef
  seq <- readMVar seqRef
  let spread = seq - cur
  when (spread > 0) $ putStrLn $ "Spread " ++ (show spread)
  reportStats curRef seqRef



process :: DB -> Cursor -> Int -> Kafka -> KafkaTopic -> IO ()
process db curRef delay kafka topic = do
  modifyMVar curRef $ \cur -> do
    (cur', events, actions) <- process' cur [] []
    when (not $ null $ events) $ do
      let messages = map (\s -> KafkaProduceMessage s) events
      let curBS'   = BL.toStrict $ B.encode cur'
      let actions' = (LDB.Put "cur" curBS') : actions
      produceMessageBatch topic KafkaUnassignedPartition messages
      runResourceT $ LDB.write db def actions'
    return (cur', ())

  threadDelay delay
  process db curRef delay kafka topic

  where
    process' :: Int -> [BS.ByteString] -> [LDB.BatchOp] -> IO (Int, [BS.ByteString], [LDB.BatchOp])
    process' cur events actions = do
      let curBS = BL.toStrict $ B.encode cur
      rawEvent <- runResourceT $ LDB.get db def curBS
      case rawEvent of
        Nothing -> return (cur, events, actions)
        (Just event) -> do
          let cur'     = cur + 1
          let events'  = event : events
          let actions' = (LDB.Del curBS) : actions
          process' cur' events' actions'



app :: DB -> Cursor -> Application
app db seqRef req respond = do
  body <- requestBody req
  modifyMVar seqRef $ \seq -> do
    let seq'   = seq + 1
    let seqBS  = BL.toStrict $ B.encode seq
    let seqBS' = BL.toStrict $ B.encode seq'
    runResourceT $
      LDB.write db def [
        LDB.Put seqBS body,  -- Write event
        LDB.Put "seq" seqBS' -- Advance cursor
      ]
    resp <- respond $
      responseLBS status200 contentTypePlain success
    return (seq', resp)

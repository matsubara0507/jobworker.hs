{-# LANGUAGE RecordWildCards #-}

module JobWorker.Client where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (when)
import qualified Data.List          as List
import qualified JobWorker.Job      as Job
import qualified JobWorker.Protocol as Protocol
import qualified Network.WebSockets as WS
import           System.IO          (hFlush, stdout)
import           Text.Read          (readMaybe)

data Client = Client
  { host    :: String
  , port    :: Int
  , path    :: String
  , verbose :: Bool
  , configs :: [Job.Config]
  }

-- dest is expected 'localhost:8080/hoge/fuga'
new :: String -> Bool -> Maybe Client
new dest verbose = do
  port <- readMaybe port'
  pure Client {..}
  where
    (addr, path)  = span (/= '/') dest
    (host, port') = drop 1 <$> span (/= ':') addr
    configs = []

run :: Client -> IO ()
run client = do
  logDebug client $ "Connecting to " ++ toDestination client
  WS.runClient client.host client.port client.path $ \conn -> do
    logDebug client $ "Connected to " ++ toDestination client
    receive conn client
    logDebug client "Close worker"

receive :: WS.Connection -> Client -> IO ()
receive conn = go
  where
    go client = do
      p <- WS.receiveData conn
      case p of
        Protocol.JobConfigs configs ->
          go (client { configs = configs })
        Protocol.Enqueue jid jname -> do
          _ <- forkIO $ runJob conn client jid jname
          go client
        _ ->
          go client

runJob :: WS.Connection -> Client -> Job.Id -> Job.Name -> IO ()
runJob conn client jid name =
  case List.find (\job -> job.name == name) client.configs of
    Nothing ->
      WS.sendBinaryData conn (Protocol.JobFailure jid)
    Just _ -> do
      WS.sendBinaryData conn (Protocol.JobRunning jid)
      logDebug client "ToDo: run"
      threadDelay 10_000_000
      WS.sendBinaryData conn (Protocol.JobSuccess jid)

toDestination :: Client -> String
toDestination client =
  client.host ++ ":" ++ show client.port ++ client.path

logDebug :: Client -> String -> IO ()
logDebug config msg =
  when config.verbose $ putStrLn msg >> hFlush stdout

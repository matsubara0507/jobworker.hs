{-# LANGUAGE RecordWildCards #-}

module JobWorker.Client where

import           Control.Concurrent     (forkIO)
import qualified Control.Concurrent.STM as STM
import           Control.Exception      (finally)
import           Control.Monad          (forever, when)
import           Data.Coerce            (coerce)
import qualified Data.List              as List
import qualified JobWorker.Docker       as Docker
import           JobWorker.Job          (Job)
import qualified JobWorker.Job          as Job
import qualified JobWorker.Protocol     as Protocol
import qualified Network.WebSockets     as WS
import           System.Exit            (ExitCode (..))
import           System.IO              (hFlush, stdout)
import           Text.Read              (readMaybe)

data Client = Client
  { host    :: String
  , port    :: Int
  , path    :: String
  , verbose :: Bool
  , configs :: STM.TVar [Job.Config]
  , queue   :: STM.TQueue Job
  }

-- dest is expected 'localhost:8080/hoge/fuga'
new :: String -> Bool -> IO (Maybe Client)
new dest verbose =
  case readMaybe port' of
    Nothing ->
      pure Nothing
    Just port -> do
      configs <- STM.newTVarIO mempty
      queue <- STM.newTQueueIO
      pure $ Just Client {..}
  where
    (addr, path)  = span (/= '/') dest
    (host, port') = drop 1 <$> span (/= ':') addr

run :: Client -> IO ()
run client = do
  logDebug client $ "Connecting to " ++ toDestination client
  WS.runClientWith client.host client.port client.path opt [] $ \conn ->
    WS.withPingThread conn 30 (logDebug client "Ping") $ do
      logDebug client $ "Connected to " ++ toDestination client
      _ <- forkIO $ forever (runJob conn client)
      forever (receive conn client) `finally` logDebug client "Close worker"
  where
    opt = WS.defaultConnectionOptions { WS.connectionOnPong = logDebug client "Pong" }

receive :: WS.Connection -> Client -> IO ()
receive conn client = do
  p <- WS.receiveData conn
  case p of
    Protocol.JobConfigs configs ->
      STM.atomically $ STM.writeTVar client.configs configs
    Protocol.Enqueue jid jname -> do
      STM.atomically $ STM.writeTQueue client.queue (Job.new jname jid)
    _ ->
      pure ()

runJob :: WS.Connection -> Client -> IO ()
runJob conn client = do
  job <- STM.atomically $ STM.readTQueue client.queue
  configs <- STM.atomically $ STM.readTVar client.configs
  case List.find (\config -> config.name == job.name) configs of
    Nothing ->
      WS.sendBinaryData conn (Protocol.JobFailure job.id)
    Just config -> do
      WS.sendBinaryData conn (Protocol.JobRunning job.id)
      logDebug client $ "Run: " ++ coerce config.name
      (code, out, err) <- Docker.run config
      when (not $ null out) $ do
        logDebug client $ "=== STDOUT ===\n" ++ out
      when (not $ null err) $ do
        logDebug client $ "=== STDERR ===\n" ++ err
      case code of
        ExitSuccess ->
          WS.sendBinaryData conn (Protocol.JobSuccess job.id)
        ExitFailure _ ->
          WS.sendBinaryData conn (Protocol.JobFailure job.id)

toDestination :: Client -> String
toDestination client =
  client.host ++ ":" ++ show client.port ++ client.path

logDebug :: Client -> String -> IO ()
logDebug config msg =
  when config.verbose $ putStrLn msg >> hFlush stdout

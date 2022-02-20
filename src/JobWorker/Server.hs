module JobWorker.Server where

import           Control.Exception      (finally)
import           Control.Monad          (forever, when)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.List              as List
import           JobWorker.DB           as DB
import           JobWorker.Job          (Job)
import qualified JobWorker.Job          as Job
import qualified JobWorker.Protocol     as Protocol
import           JobWorker.Worker       as Worker
import qualified Network.WebSockets     as WS
import           Servant
import qualified Servant.API.WebSocket  as WS
import           System.IO              (hFlush, stdout)
import           System.Random          (randomRIO)

data Config = Config
  { verbose :: Bool
  , job     :: [Job.Config]
  }

type API
    = "api" :> JobAPI
 :<|> "runner" :> WS.WebSocket

api :: Proxy API
api = Proxy

type JobAPI
    = "workers" :> Get '[JSON] [Worker.Info]
 :<|> "jobs" :> Get '[JSON] [Job]
 :<|> "jobs" :> Capture "name" Job.Name :> Post '[JSON] Job

server :: Config -> DB -> Server API
server config db
  = (getWorkers :<|> getJobs :<|> kickJob) :<|> (liftIO . serveRunner config db)
  where
    getWorkers = liftIO $ DB.getAllWorkerInfo db
    getJobs = liftIO $ DB.getAllJob db
    kickJob name =
      case List.find (\job -> job.name == name) config.job of
        Nothing ->
          throwError err404
        Just _ -> do
          w <- liftIO $ randomWorker db
          case w of
            Nothing ->
              throwError $ err500 { errBody = "worker is not exist." }
            Just worker -> liftIO $ do
              job <- DB.enqueueJob db name
              WS.sendBinaryData worker.conn (Protocol.Enqueue job.id job.name)
              pure job

serveRunner :: Config -> DB -> WS.Connection -> IO ()
serveRunner config db conn = do
  worker <- DB.connectedWorker db conn
  logDebug config $ "Connected worker " ++ show worker.id
  WS.sendBinaryData worker.conn (Protocol.JobConfigs config.job)
  logDebug config $ "Setuped worker " ++ show worker.id
  _ <- forever (receive worker) `finally` DB.disconnectedWorker db worker.id
  logDebug config $ "Close worker " ++ show worker.id
  where
    receive worker = do
      p <- WS.receiveData worker.conn
      case p of
        Protocol.JobRunning jid ->
          DB.runningJob db worker.id jid
        Protocol.JobSuccess jid ->
          DB.successJob db worker.id jid
        Protocol.JobFailure jid ->
          DB.failureJob db worker.id jid
        _ ->
          pure ()

randomWorker :: DB -> IO (Maybe Worker)
randomWorker db = do
  workers <- DB.getAllWorker db
  case workers of
    [] ->
      pure Nothing
    _ -> do
      idx <- (\x -> x - 1) <$> randomRIO (1, length workers)
      pure $ Just (workers !! idx)

logDebug :: Config -> String -> IO ()
logDebug config msg =
  when config.verbose $ putStrLn msg >> hFlush stdout

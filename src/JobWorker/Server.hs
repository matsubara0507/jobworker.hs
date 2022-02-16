module JobWorker.Server where

import           Control.Concurrent     (threadDelay)
import           Control.Exception      (finally)
import           Control.Monad          (forever, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text              as Text
import           JobWorker.DB           as DB
import           JobWorker.Worker       as Worker
import qualified Network.WebSockets     as WS
import           Servant
import qualified Servant.API.WebSocket  as WS

data Config = Config
  { verbose :: Bool
  }

type API
    = "api" :> JobAPI
 :<|> "runner" :> WS.WebSocket

api :: Proxy API
api = Proxy

type JobAPI
    = "workers" :> Get '[JSON] [Worker.Info]

server :: Config -> DB -> Server API
server config db = getWorkers :<|> serveRunner config db
  where
    getWorkers = liftIO $ DB.getAllWorkerInfo db

serveRunner :: MonadIO m => Config -> DB -> WS.Connection -> m ()
serveRunner config db conn = liftIO $ do
  worker <- DB.connectedWorker db conn
  when config.verbose $
    putStrLn ("connected worker " ++ show worker.id)
  go worker `finally` DB.disconnectedWorker db worker
  where
    go worker = do
      WS.sendTextData worker.conn (Text.pack $ show worker.id)
      _ <- forever (threadDelay 1000000)
      pure ()

{-# LANGUAGE RecordWildCards #-}

module JobWorker.DB where

import qualified Control.Concurrent.STM as STM
import           Data.Coerce            (coerce)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (catMaybes)
import           JobWorker.Worker       as Worker
import qualified Network.WebSockets     as WS

data DB = DB
  { workers :: STM.TVar (Map Worker.Id (Maybe Worker))
  }

new :: IO DB
new = do
  workers <- STM.newTVarIO mempty
  pure DB {..}

getAllWorkerInfo :: DB -> IO [Worker.Info]
getAllWorkerInfo db =
  map Worker.toInfo . catMaybes . Map.elems <$> STM.readTVarIO db.workers

connectedWorker :: DB -> WS.Connection -> IO Worker
connectedWorker db conn = STM.atomically $ do
  maxId <- Map.size <$> STM.readTVar db.workers
  let worker = Worker.new (coerce $ maxId + 1) conn
  STM.modifyTVar db.workers (Map.insert worker.id $ Just worker)
  pure worker

disconnectedWorker :: DB -> Worker -> IO ()
disconnectedWorker db worker =
  STM.atomically $ STM.modifyTVar db.workers (Map.update (\_ -> Just Nothing) worker.id)

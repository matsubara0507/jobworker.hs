{-# LANGUAGE RecordWildCards #-}

module JobWorker.DB where

import qualified Control.Concurrent.STM as STM
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (catMaybes)
import           JobWorker.Job          as Job
import           JobWorker.Worker       as Worker
import qualified Network.WebSockets     as WS

data DB = DB
  { workers :: STM.TVar (Map Worker.Id (Maybe Worker))
  , jobs    :: STM.TVar (Map Job.Id Job)
  }

new :: IO DB
new = do
  workers <- STM.newTVarIO mempty
  jobs <- STM.newTVarIO mempty
  pure DB {..}

getAllWorker :: DB -> IO [Worker]
getAllWorker db = catMaybes . Map.elems <$> STM.readTVarIO db.workers

getAllWorkerInfo :: DB -> IO [Worker.Info]
getAllWorkerInfo db = map Worker.toInfo <$> getAllWorker db

connectedWorker :: DB -> WS.Connection -> IO Worker
connectedWorker db conn = STM.atomically $ do
  maxId <- Map.size <$> STM.readTVar db.workers
  let worker = Worker.new (fromIntegral $ maxId + 1) conn
  STM.modifyTVar db.workers (Map.insert worker.id $ Just worker)
  pure worker

disconnectedWorker :: DB -> Worker.Id -> IO ()
disconnectedWorker db wid =
  STM.atomically $ STM.modifyTVar db.workers (Map.update (\_ -> Just Nothing) wid)

getAllJob :: DB -> IO [Job]
getAllJob db =
  Map.elems <$> STM.readTVarIO db.jobs

enqueueJob :: DB -> Job.Name -> IO Job
enqueueJob db name = STM.atomically $ do
  maxId <- Map.size <$> STM.readTVar db.jobs
  let job = Job.new name (fromIntegral $ maxId + 1)
  STM.modifyTVar db.jobs (Map.insert job.id job)
  pure job

runningJob, successJob, failureJob :: DB -> Worker.Id -> Job.Id -> IO ()
runningJob db wid jid = STM.atomically $ do
  STM.modifyTVar db.workers (Map.update (Just . fmap Worker.work) wid)
  STM.modifyTVar db.jobs (Map.update (Just . Job.updateToRun) jid)
successJob db wid jid = STM.atomically $ do
  STM.modifyTVar db.workers (Map.update (Just . fmap Worker.finish) wid)
  STM.modifyTVar db.jobs (Map.update (Just . Job.updateToSuccess) jid)
failureJob db wid jid = STM.atomically $ do
  STM.modifyTVar db.workers (Map.update (Just . fmap Worker.finish) wid)
  STM.modifyTVar db.jobs (Map.update (Just . Job.updateToFailure) jid)

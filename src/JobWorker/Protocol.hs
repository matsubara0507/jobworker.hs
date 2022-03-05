module JobWorker.Protocol where

import qualified Data.Aeson           as JSON
import qualified Data.Binary          as Binary
import qualified Data.ByteString.Lazy as LBS
import qualified JobWorker.Job        as Job
import qualified Network.WebSockets   as WS

-- | protocol from Server to Client
data Server
  = JobConfigs [Job.Config]
  | Enqueue Job.Id Job.Name
  | SUndefined

instance WS.WebSocketsData Server where
  fromDataMessage (WS.Text   bl _) = WS.fromLazyByteString bl
  fromDataMessage (WS.Binary bl)   = WS.fromLazyByteString bl

  fromLazyByteString lbs = case LBS.uncons lbs of
    Just (1, rest) ->
      case JSON.decode rest of
        Just configs ->
          JobConfigs configs
        Nothing ->
          SUndefined

    Just (2, rest) ->
      uncurry Enqueue (Binary.decode rest)

    _ ->
      SUndefined

  toLazyByteString p = case p of
    JobConfigs configs ->
      LBS.cons 1 (JSON.encode configs)

    Enqueue wid name ->
      LBS.cons 2 (Binary.encode (wid, name))

    SUndefined ->
      ""

-- | protocol from Client to Server
data Client
  = JobRunning Job.Id
  | JobSuccess Job.Id
  | JobFailure Job.Id
  | CUndefined

instance WS.WebSocketsData Client where
  fromDataMessage (WS.Text   bl _) = WS.fromLazyByteString bl
  fromDataMessage (WS.Binary bl)   = WS.fromLazyByteString bl

  fromLazyByteString lbs = case LBS.uncons lbs of
    Just (1, rest) ->
      JobRunning (Binary.decode rest)

    Just (2, rest) ->
      JobSuccess (Binary.decode rest)

    Just (3, rest) ->
      JobFailure (Binary.decode rest)

    _ ->
      CUndefined

  toLazyByteString p = case p of
    JobRunning wid ->
      LBS.cons 1 (Binary.encode wid)

    JobSuccess wid ->
      LBS.cons 2 (Binary.encode wid)

    JobFailure wid ->
      LBS.cons 3 (Binary.encode wid)

    CUndefined ->
      ""

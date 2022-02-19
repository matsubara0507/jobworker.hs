{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}

module JobWorker.Worker where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Binary        (Binary)
import           Data.Int           (Int32)
import           GHC.Generics       (Generic)
import qualified Network.WebSockets as WS

newtype Id = Id Int32
  deriving newtype (Show, Eq, Ord, Num, Binary, FromJSON, ToJSON)

data Worker = Worker
  { id      :: Id
  , conn    :: WS.Connection
  , working :: Bool
  }

new :: Id -> WS.Connection -> Worker
new wid conn = Worker wid conn False

work, finish :: Worker -> Worker
work w = w { working = True }
finish w = w { working = False }

data Info = Info
  { id      :: Id
  , working :: Bool
  } deriving (Generic, Show, Eq)

instance ToJSON Info
instance FromJSON Info

toInfo :: Worker -> Info
toInfo w = Info { id = w.id, working = w.working}

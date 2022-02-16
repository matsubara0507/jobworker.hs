{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}

module JobWorker.Worker where

import           Data.Aeson         (FromJSON, ToJSON)
import           GHC.Generics       (Generic)
import qualified Network.WebSockets as WS

newtype Id = Id Int
  deriving newtype (Show, Eq, Ord, FromJSON, ToJSON)

data Worker = Worker
  { id      :: Id
  , conn    :: WS.Connection
  , working :: Bool
  }

new :: Id -> WS.Connection -> Worker
new wid conn = Worker wid conn False

data Info = Info
  { id      :: Id
  , working :: Bool
  } deriving (Generic, Show, Eq)

instance ToJSON Info
instance FromJSON Info

toInfo :: Worker -> Info
toInfo w = Info { id = w.id, working = w.working}

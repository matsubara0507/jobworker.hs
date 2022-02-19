{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}

module JobWorker.Job where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Binary     (Binary)
import           Data.Int        (Int32)
import           GHC.Generics    (Generic)
import           Web.HttpApiData (FromHttpApiData)

newtype Id = Id Int32
  deriving newtype (Show, Eq, Ord, Num, Binary, FromJSON, ToJSON)

newtype Name = Name String
  deriving newtype (Show, Eq, Ord, Binary, FromJSON, ToJSON, FromHttpApiData)

data Job = Job
  { id      :: Id
  , name    :: Name
  , queuing :: Bool
  , running :: Bool
  , success :: Bool
  } deriving (Generic, Show, Eq)

instance ToJSON Job
instance FromJSON Job

new :: Name -> Id -> Job
new name jid = Job jid name True False False

updateToRun, updateToSuccess, updateToFailure :: Job -> Job
updateToRun job = job { queuing = False, running = True }
updateToSuccess job = job { queuing = False, running = False, success = True }
updateToFailure job = job { queuing = False, running = False, success = False }

data Config = Config
  { name    :: Name
  , image   :: String
  , command :: Maybe String
  } deriving (Generic, Show, Eq)

instance ToJSON Config
instance FromJSON Config
